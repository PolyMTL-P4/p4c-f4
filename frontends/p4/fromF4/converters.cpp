/*
Copyright 2013-present Barefoot Networks, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#include "converters.h"
#include "frontends/common/constantFolding.h"
#include "frontends/common/options.h"
#include "frontends/common/parser_options.h"
#include "frontends/common/resolveReferences/referenceMap.h"
#include "frontends/common/resolveReferences/resolveReferences.h"
#include "frontends/p4/coreLibrary.h"
#include "ir/indexed_vector.h"
#include "ir/ir-generated.h"
#include "lib/big_int_util.h"
#include "lib/cstring.h"
#include "nlohmann/json.hpp"
#include <cstring>
#include <fstream>
#include <map>
#include <vector>

using json = nlohmann::json;

namespace F4 {

///////////////////////////////////////////////////////////////

const IR::Node *ReplaceMembers::preorder(IR::Member *IRmember) {
    auto exprName = IRmember->expr->toString();
    auto memberName = IRmember->member.toString();
    if (auto found = instanceToClassNameMap->find(exprName); found != instanceToClassNameMap->end()) {
        auto finalName = memberName + "_" + found->second + "_" + exprName;
        return new IR::PathExpression(IR::ID(finalName));
    }
    return IRmember;
}

const IR::Node *ReplaceParameters::postorder(IR::Expression *expr) {
    if (auto argFound = paramArgMap->find(expr->toString()); argFound != paramArgMap->end()) {
        const auto *newExpr = argFound->second.expression;
        return newExpr;
    }
    if (auto newNameFound = substituteVars->find(expr->toString()); newNameFound != substituteVars->end()) {
        auto *newPath = new IR::PathExpression(IR::ID(newNameFound->second));
        return newPath;
    }
    return expr;
}

const IR::Node *RegisterClass::preorder(IR::P4Class *laClass) {
    ClassSettings settings; 
    settings.decls = laClass->localDeclarations;
    settings.params = *(laClass->getParameters());
    classSettingsMap->emplace(laClass->name.toString(), settings);
    return nullptr;
}

const IR::Node *ExtendP4Class::preorder(IR::Declaration_Instance *object) {
    const cstring className = object->type->toString();
    const cstring instanceName = object->Name();
    std::map<cstring, IR::Argument> paramArgMap;
    std::map<cstring, cstring> substituteVars;

    if (auto settingsFound = classSettingsMap->find(className); settingsFound != classSettingsMap->end()) {
        ClassSettings settings = settingsFound->second;
        auto paramsVector = settings.params.parameters;
        for (size_t i = 0; i < paramsVector.size(); i++) {
            paramArgMap.emplace(paramsVector.at(i)->name.toString(), *(object->arguments->at(i)));
        }
        instanceToClassNameMap->emplace(instanceName, className);
        auto *renamedDecls = addNameToDecls(&settings.decls, className, instanceName, &substituteVars);
        const auto *result = renamedDecls->apply(ReplaceParameters(&paramArgMap, &substituteVars));
        return result;
    }
        return object;
}

const IR::Node *ExtendP4Class::preorder(IR::Expression *call) {
    const auto *newCall = call->clone();
    newCall = newCall->apply(ReplaceMembers(instanceToClassNameMap));
    return newCall;
}

IR::IndexedVector<IR::Declaration> *ExtendP4Class::addNameToDecls (IR::IndexedVector<IR::Declaration> *ref,
                                                cstring className, cstring instanceName,
                                                std::map<cstring, cstring> *substituteVars) {
    auto *result = new IR::IndexedVector<IR::Declaration>();
    for (const auto *d : *ref) {
        auto newName = d->name.toString() + "_" + className + "_" + instanceName;
        substituteVars->emplace(d->name.toString(), newName);
        auto *newDecl = d->clone();
        newDecl->setName(IR::ID(newName));
        result->push_back(newDecl);
    }
    return result;
}

const IR::Node *EfsmToFlowBlaze::preorder(IR::P4Efsm *efsm) {

    json fbTotal;

    for (const auto *state : efsm->states) {

        cstring srcState = state->name.toString();
        fbTotal["nodes"].push_back({{"x", 0},
                                    {"y", 0},
                                    {"text", srcState},
                                    {"isAcceptState", false}});

        cstring regUpdate = "";
        cstring lesActions = "";
        for (const auto *ligne : state->components) {
            if (ligne->is<IR::MethodCallStatement>()) {
                lesActions += ligne->toString() + "()";
            } else {
                regUpdate += ligne->toString();
            }
        }
        if (state->selectExpression->is<IR::SelectExpression>()) {
            auto selectExpr = state->selectExpression->as<IR::SelectExpression>();
            for (const auto *sCase : selectExpr.selectCases) {
                cstring transitionSymbol = sCase->keyset->toString();
                cstring dstState = sCase->state->toString();
                if (!sCase->keyset->is<IR::DefaultExpression>()) {
                    // Only true/false select expression are authorized, for now
                    auto *condition = selectExpr.select->components.at(0);
                    if (!strcmp(transitionSymbol, "false")) {
                        //condition = "the other way plz";
                        condition->as<IR::Operation_Relation>();
                        if (condition->is<IR::Lss>()) {
                            auto truc = condition->as<IR::Lss>();
                            condition = new IR::Geq(truc.left, truc.right);
                        }
                    }
                    cstring condStr = condition->toString();

                    if (strcmp(srcState, dstState) != 0) {
                        fbTotal["links"].push_back({{"type", "Link"},
                                                    {"nodeA", srcState},
                                                    {"nodeB", dstState},
                                                    {"text", "| " + condStr + " | " + regUpdate + " | " + lesActions},
                                                    {"lineAngleAdjust", 0},
                                                    {"parallelPart", 0},
                                                    {"perpendicularPart", 0}});
                    } else {
                        fbTotal["links"].push_back({{"type", "SelfLink"},
                                                    {"nodeA", srcState},
                                                    {"text", "| " + condStr + " | " + regUpdate + " | " + lesActions},
                                                    {"anchorAngle", 0}});
                    }
                }
            }
        } else if (state->selectExpression->is<IR::PathExpression>()) {
            cstring dstState = state->selectExpression->toString();
            if (strcmp(srcState, dstState) != 0) {
                fbTotal["links"].push_back({{"type", "Link"},
                                            {"nodeA", srcState},
                                            {"nodeB", dstState},
                                            {"text", " | | " + regUpdate + " | " + lesActions},
                                            {"lineAngleAdjust", 0},
                                            {"parallelPart", 0},
                                            {"perpendicularPart", 0}});
            } else {
                fbTotal["links"].push_back({{"type", "SelfLink"},
                                            {"nodeA", srcState},
                                            {"text", "| | " + regUpdate + " | " + lesActions},
                                            {"anchorAngle", 0}});
            }
        }
    }

    std::ofstream o("for-flowblaze.json");
    o << std::setw(4) << fbTotal << std::endl;

    return nullptr;
}

const IR::Node *EfsmToDfaSynthesis::preorder(IR::P4Efsm *efsm) {

    json dfaTotal;
    std::vector<cstring> sigma;

    dfaTotal["initial"] = "start";

    for (const auto *state : efsm->states) {

        dfaTotal["states"].push_back(state->name.toString());
        if (state->selectExpression->is<IR::SelectExpression>()) {
            for (const auto *const sCase : state->selectExpression->as<IR::SelectExpression>().selectCases) {
                cstring transitionSymbol = sCase->keyset->toString();
                if (!sCase->keyset->is<IR::DefaultExpression>()) {
                    if (std::find(sigma.begin(), sigma.end(), transitionSymbol) == sigma.end()) {
                        sigma.push_back(transitionSymbol);
                        dfaTotal["sigma"].push_back(transitionSymbol);
                    }
                dfaTotal["transitions"].push_back(std::vector<cstring> {state->name.toString(), transitionSymbol, sCase->state->toString()});
                }
            }
        }
    }

    dfaTotal["accepting"].push_back(dfaTotal["states"].at(dfaTotal["states"].size() - 1));

    std::ofstream o("for-dfa-synthesis.json");
    o << std::setw(4) << dfaTotal << std::endl;

    return nullptr;
}

Converter::Converter(ParserOptions::EfsmBackendType efsmBackend) : classSettingsMap(new std::map<cstring, ClassSettings>()),
                         instanceToClassNameMap(new std::map<cstring, cstring>()) {
    setName("Converter");

    passes.emplace_back(new RegisterClass(classSettingsMap));
    passes.emplace_back(new ExtendP4Class(classSettingsMap, instanceToClassNameMap));
    if (efsmBackend == ParserOptions::EfsmBackendType::FLOWBLAZE_P4) {
        passes.emplace_back(new EfsmToFlowBlaze());
    } else if (efsmBackend == ParserOptions::EfsmBackendType::DFA_SYNTHESIS) {
        passes.emplace_back(new EfsmToDfaSynthesis());
    }

}

Visitor::profile_t Converter::init_apply(const IR::Node *node) {
    if (!node->is<IR::P4Program>()) { BUG("Converter only accepts IR::Globals, not %1%", node);
}
    return PassManager::init_apply(node);
}

} // namespace F4
