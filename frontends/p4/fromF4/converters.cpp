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
#include <map>

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
