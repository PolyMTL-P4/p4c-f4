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

#include <array>
#include <fstream>

/*#include <algorithm>
#include <bitset>
#include <cstddef>
#include <cstring>
#include <iomanip>
#include <iterator>
#include <map>
#include <sstream>
#include <string>
#include <vector>

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
#include "lib/exceptions.h"*/

enum {
    MAX_REG_ACTIONS_PER_TRANSITION = 3,
    MAX_CONDITIONS = 4,
    FDV_BASE_REGISTER = 0x00,
    GDV_BASE_REGISTER = 0x0F,
    REVERSE_CONDITION_XOR = 0b111,
    SWAP_CONDITION_XOR = 0b110
};

struct RegActPar {
    cstring operation;
    std::string result;
    std::string op1;
    std::string op2;
    std::string operand1;
    std::string operand2;
};

struct CondPar {
    uint cond;
    std::string op1;
    std::string op2;
    std::string operand1;
    std::string operand2;
    std::string matchVariable;
    bool operator==(const CondPar &laCond) const {
        return ((cond == laCond.cond) && (op1 == laCond.op1) && (op2 == laCond.op2) &&
                (operand1 == laCond.operand1) && (operand2 == laCond.operand2) &&
                (matchVariable == laCond.matchVariable)) ||
               (((cond ^ SWAP_CONDITION_XOR) == laCond.cond) && (op2 == laCond.op1) &&
                (op1 == laCond.op2) && (operand2 == laCond.operand1) &&
                (operand1 == laCond.operand2) && (matchVariable == laCond.matchVariable));
    }
};

namespace F4 {

///////////////////////////////////////////////////////////////

const IR::Node *ReplaceMembers::preorder(IR::Member *IRmember) {
    auto exprName = IRmember->expr->toString();
    auto memberName = IRmember->member.toString();
    if (auto found = instanceToClassNameMap->find(exprName);
        found != instanceToClassNameMap->end()) {
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
    if (auto newNameFound = substituteVars->find(expr->toString());
        newNameFound != substituteVars->end()) {
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

    if (auto settingsFound = classSettingsMap->find(className);
        settingsFound != classSettingsMap->end()) {
        ClassSettings settings = settingsFound->second;
        auto paramsVector = settings.params.parameters;
        for (size_t i = 0; i < paramsVector.size(); i++) {
            paramArgMap.emplace(paramsVector.at(i)->name.toString(), *(object->arguments->at(i)));
        }
        instanceToClassNameMap->emplace(instanceName, className);
        auto *renamedDecls =
            addNameToDecls(&settings.decls, className, instanceName, &substituteVars);
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

IR::IndexedVector<IR::Declaration> *ExtendP4Class::addNameToDecls(
    IR::IndexedVector<IR::Declaration> *ref, cstring className, cstring instanceName,
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

cstring methodCallToString(const IR::MethodCallStatement *methodCall) {
    cstring callStr = "";
    const auto *argsOfCall = methodCall->methodCall->arguments;
    callStr += methodCall->methodCall->method->toString() + "(";
    if (!argsOfCall->empty()) {
        for (size_t i = 0; i < argsOfCall->size() - 1; i++) {
            callStr += argsOfCall->at(i)->toString() + ",";
        }
        callStr += argsOfCall->at(argsOfCall->size() - 1)->toString() + ")";
    } else {
        callStr += ")";
    }
    return callStr;
}

template <typename T>
int findPositionInVector(std::vector<T> &leVecteur, T &lElement) {
    int elemPos = 0;
    auto elemFound = std::find(leVecteur.begin(), leVecteur.end(), lElement);
    if (elemFound != leVecteur.end()) {
        elemPos = elemFound - leVecteur.begin();
    } else {
        BUG("Element not found\n");
    }
    return elemPos;
}

template <typename T>
void insertIfNotInVec(std::vector<T> &leVecteur, T lElement) {
    if (std::find(leVecteur.begin(), leVecteur.end(), lElement) == leVecteur.end()) {
        leVecteur.push_back(lElement);
    }
}

cstring intToHexStr(int number) {
    std::ostringstream oss;
    oss << "0x" << std::hex << number;
    return oss.str();
}

cstring intToBinStr(uint number) {
    std::bitset<sizeof(number)> bs(number);
    return ("0b" + bs.to_string());
}

void parseActionCall(const IR::MethodCallStatement *actionCall,
                     std::vector<std::pair<cstring, IR::MethodCallStatement>> &calledPktActionsMap,
                     int &currentPktAction) {
    auto it = find_if(calledPktActionsMap.begin(), calledPktActionsMap.end(),
                      [actionCall](const std::pair<cstring, IR::MethodCallStatement> &p) {
                          return p.first == methodCallToString(actionCall);
                      });
    if (it == calledPktActionsMap.end()) {
        calledPktActionsMap.emplace_back(methodCallToString(actionCall), *actionCall);
        currentPktAction = static_cast<int>(calledPktActionsMap.size()) - 1;
    } else {
        currentPktAction = static_cast<int>(std::distance(calledPktActionsMap.begin(), it));
    }
    currentPktAction = currentPktAction + 1;
}

void fillActionParsed(const IR::Expression *expr, std::string &opX, std::string &operandX,
                      std::vector<cstring> &globalDataVariables,
                      std::vector<cstring> &flowDataVariables,
                      const std::map<cstring, int> &registers) {
    cstring laStr = expr->toString();
    if (expr->is<IR::Constant>()) {
        opX = intToHexStr(registers.at("EXPL"));
        operandX = laStr;
    } else if (strncmp(laStr, "#", 1) == 0) {
        insertIfNotInVec(globalDataVariables, laStr);
        uint elemPos = findPositionInVector(globalDataVariables, laStr);
        opX = intToHexStr(static_cast<int>(GDV_BASE_REGISTER + (elemPos << 4U)));
        operandX = "0";
    } else if ((strcmp(laStr, "now") == 0) || (strcmp(laStr, "meta") == 0)) {
        opX = intToHexStr(registers.at(laStr));
        operandX = "0";
    } else {
        insertIfNotInVec(flowDataVariables, laStr);
        int elemPos = findPositionInVector(flowDataVariables, laStr);
        opX = intToHexStr(FDV_BASE_REGISTER + elemPos);
        operandX = "0";
    }
}

void parseRegAction(RegActPar &actionParsed, const IR::AssignmentStatement *varAss,
                    std::vector<cstring> &globalDataVariables,
                    std::vector<cstring> &flowDataVariables,
                    const std::map<cstring, int> &operations,
                    const std::map<cstring, int> &registers) {
    cstring res = varAss->left->toString();
    if (strncmp(res, "#", 1) == 0) {
        insertIfNotInVec(globalDataVariables, res);
        uint elemPos = findPositionInVector(globalDataVariables, res);
        actionParsed.result = intToHexStr(static_cast<int>(GDV_BASE_REGISTER + (elemPos << 4U)));
    } else {
        insertIfNotInVec(flowDataVariables, res);
        int elemPos = findPositionInVector(flowDataVariables, res);
        actionParsed.result = intToHexStr(FDV_BASE_REGISTER + elemPos);
    }
    if (varAss->right->is<IR::Operation_Binary>()) {
        const auto *opTotal = varAss->right->to<IR::Operation_Binary>();
        fillActionParsed(opTotal->left, actionParsed.op1, actionParsed.operand1,
                         globalDataVariables, flowDataVariables, registers);
        actionParsed.operation = intToHexStr(operations.at(opTotal->getStringOp()));
        fillActionParsed(opTotal->right, actionParsed.op2, actionParsed.operand2,
                         globalDataVariables, flowDataVariables, registers);
    } else {
        fillActionParsed(varAss->right, actionParsed.op1, actionParsed.operand1,
                         globalDataVariables, flowDataVariables, registers);
        actionParsed.operation = intToHexStr(operations.at("+"));
        actionParsed.op2 = intToHexStr(registers.at("EXPL"));
        actionParsed.operand2 = "0";
    }
}

void parseAction(const IR::StatOrDecl *ligne,
                 std::vector<std::pair<cstring, IR::MethodCallStatement>> &calledPktActionsMap,
                 std::vector<RegActPar> &regActionsParsed,
                 std::vector<cstring> &globalDataVariables, std::vector<cstring> &flowDataVariables,
                 const std::map<cstring, int> &operations, const std::map<cstring, int> &registers,
                 int &currentPktAction) {
    if (ligne->is<IR::MethodCallStatement>()) {
        parseActionCall(ligne->to<IR::MethodCallStatement>(), calledPktActionsMap,
                        currentPktAction);
    } else if (ligne->is<IR::AssignmentStatement>()) {
        RegActPar actionParsed = {"0", "0", "0", "0", "0", "0"};
        parseRegAction(actionParsed, ligne->to<IR::AssignmentStatement>(), globalDataVariables,
                       flowDataVariables, operations, registers);
        regActionsParsed.emplace_back(actionParsed);
    } else {
        cstring errorMsg = "Non identified statement: " + ligne->toString();
        BUG(errorMsg);
    }
}

void parseConditionOrMatch(const IR::SelectExpression &selectExpr, std::vector<CondPar> &selectArgs,
                           const std::map<cstring, uint> &conditions,
                           std::vector<cstring> &globalDataVariables,
                           std::vector<cstring> &flowDataVariables,
                           const std::map<cstring, int> &registers) {
    for (const auto *conditionOrMatch : selectExpr.select->components) {
        CondPar conditionParsed = {0, "0", "0", "0", "0", ""};
        if (conditionOrMatch->is<IR::Operation_Relation>()) {
            const auto *cond = conditionOrMatch->to<IR::Operation_Relation>();
            conditionParsed.cond = conditions.at(cond->getStringOp());
            fillActionParsed(cond->left, conditionParsed.op1, conditionParsed.operand1,
                             globalDataVariables, flowDataVariables, registers);
            fillActionParsed(cond->right, conditionParsed.op2, conditionParsed.operand2,
                             globalDataVariables, flowDataVariables, registers);
        } else {
            conditionParsed.matchVariable = conditionOrMatch->toString();
        }
        selectArgs.push_back(conditionParsed);
    }
}

void insertCondition(const CondPar &conditionTrue, std::vector<CondPar> &conditionsParsedVec,
                     std::array<bool, 4> &currentConditions, bool reverseCondition = false) {
    uint cond = reverseCondition ? conditionTrue.cond ^ REVERSE_CONDITION_XOR : conditionTrue.cond;
    CondPar conditionParsed = {cond,
                               conditionTrue.op1,
                               conditionTrue.op2,
                               conditionTrue.operand1,
                               conditionTrue.operand2,
                               conditionTrue.matchVariable};
    insertIfNotInVec(conditionsParsedVec, conditionParsed);
    currentConditions.at(findPositionInVector(conditionsParsedVec, conditionParsed)) = true;
}

void parseKeyset(const IR::Expression *expr, CondPar &selectArg,
                 std::vector<CondPar> &conditionsParsedVec, std::array<bool, 4> &currentConditions,
                 std::vector<std::string> &otherMatches, cstring &matchStr) {
    bool defaultCase = expr->is<IR::DefaultExpression>();
    cstring transitionSymbol = expr->toString();
    if (selectArg.matchVariable.empty()) {
        if (!defaultCase) {
            if (strcmp(transitionSymbol, "true") == 0) {
                insertCondition(selectArg, conditionsParsedVec, currentConditions, false);
            } else if (strcmp(transitionSymbol, "false") == 0) {
                insertCondition(selectArg, conditionsParsedVec, currentConditions, true);
            }
        }
    } else {
        if (defaultCase) {
            matchStr += "0&&&0 ";
        } else {
            insertIfNotInVec(otherMatches, selectArg.matchVariable);
            cstring matchValue = transitionSymbol;
            cstring mask = "0x";
            for (size_t i = 0; i < strlen(matchValue); i++) {
                mask += "F";
            }
            matchStr += matchValue + "&&&" + mask + " ";
        }
    }
}

cstring formatActionParsed(std::vector<RegActPar> &regActionsParsedVec,
                           const std::map<cstring, int> &operations) {
    cstring actionsForEfsm = "";
    for (size_t i = 0; i < MAX_REG_ACTIONS_PER_TRANSITION; i++) {
        if (i < regActionsParsedVec.size()) {
            auto regActionParsed = regActionsParsedVec.at(i);
            actionsForEfsm += regActionParsed.operation + " " + regActionParsed.result + " " +
                              regActionParsed.op1 + " " + regActionParsed.op2 + " " +
                              regActionParsed.operand1 + " " + regActionParsed.operand2 + " ";
        } else {
            actionsForEfsm += intToHexStr(operations.at("NOP")) + " 0 0 0 0 0 ";
        }
    }
    return actionsForEfsm;
}

cstring formatConditionParsed(std::vector<CondPar> &conditionsParsedVec,
                              const std::map<cstring, uint> &conditions) {
    cstring conditionEntry = "table_set_default FlowBlaze.condition_table set_condition_fields ";
    for (size_t i = 0; i < MAX_CONDITIONS; i++) {
        if (i < conditionsParsedVec.size()) {
            auto conditionParsed = conditionsParsedVec.at(i);
            conditionEntry += intToBinStr(conditionParsed.cond) + " " + conditionParsed.op1 + " " +
                              conditionParsed.op2 + " " + conditionParsed.operand1 + " " +
                              conditionParsed.operand2 + " ";
        } else {
            conditionEntry += intToBinStr(conditions.at("NOP")) + " 0 0 0 0 ";
        }
    }
    return conditionEntry;
}

cstring formatPktActionEntries(
    std::vector<std::pair<cstring, IR::MethodCallStatement>> &calledPktActionsMap) {
    cstring pktActionsTableEntries = "";
    for (int i = 0; static_cast<size_t>(i) < calledPktActionsMap.size(); i++) {
        auto actionCall = calledPktActionsMap.at(i).second;
        cstring argStr = "";
        for (const auto *argument : *actionCall.methodCall->arguments) {
            argStr += argument->toString() + " ";
        }
        cstring pktActionEntry = "table_add FlowBlaze.pkt_action " +
                                 actionCall.methodCall->method->toString() + " " +
                                 intToHexStr(i + 1) + "&&&0xFF => "  // action_match
                                 + argStr                            // action parameters
                                 + "10"                              // priority
                                 + "\n";
        pktActionsTableEntries += pktActionEntry;
    }
    return pktActionsTableEntries;
}

cstring formatEfsmTableCommand(int &srcStateNum, int &dstStateNum,
                               std::array<bool, 4> &currentConditions, int &currentPktAction,
                               cstring &actionsForEfsm, cstring &otherMatch) {
    cstring efsmTableCommand = "table_add FlowBlaze.EFSM_table define_operation_update_state ";
    cstring conditionList = "";
    for (int i = 0; i < 4; i++) {
        conditionList += currentConditions.at(i) ? "1&&&1 " : "0&&&0 ";
    }
    efsmTableCommand += std::to_string(srcStateNum) + "&&&0xFFFF " + conditionList + otherMatch +
                        " => " + std::to_string(dstStateNum) + " " + actionsForEfsm +
                        std::to_string(currentPktAction) + " 1"  // priority
                        + "\n";
    return efsmTableCommand;
}

void parseTransition(
    const IR::Expression *selectExpression, const std::map<cstring, uint> &conditions,
    std::vector<CondPar> &conditionsParsedVec, std::vector<cstring> &globalDataVariables,
    std::vector<cstring> &flowDataVariables, std::vector<RegActPar> &regActionsParsedVec,
    std::vector<cstring> stateStringToNum, const std::map<cstring, int> &operations,
    const std::map<cstring, int> &registers, std::vector<std::string> &otherMatches,
    cstring &efsmTableCommands, int &currentPktAction, int &srcStateNum) {
    if (selectExpression->is<IR::SelectExpression>()) {
        auto selectExpr = selectExpression->as<IR::SelectExpression>();
        std::vector<CondPar> selectArgs;

        parseConditionOrMatch(selectExpr, selectArgs, conditions, globalDataVariables,
                              flowDataVariables, registers);

        for (const auto *sCase : selectExpr.selectCases) {
            cstring matchStr = "";
            std::array<bool, 4 /*MAX_CONDITIONS*/> currentConditions = {false, false, false, false};
            // cstring transitionSymbol = sCase->keyset->toString();
            if (sCase->keyset->is<IR::ListExpression>()) {
                auto lesExpr = sCase->keyset->as<IR::ListExpression>();
                int i = 0;
                for (const auto *expr : lesExpr.components) {
                    parseKeyset(expr, selectArgs.at(i), conditionsParsedVec, currentConditions,
                                otherMatches, matchStr);
                    i++;
                }
            } else {
                parseKeyset(sCase->keyset, selectArgs.at(0), conditionsParsedVec, currentConditions,
                            otherMatches, matchStr);
            }
            cstring dstState = sCase->state->toString();
            int dstStateNum = findPositionInVector(stateStringToNum, dstState);
            cstring actionsForEfsm = formatActionParsed(regActionsParsedVec, operations);
            cstring efsmTableCommand =
                formatEfsmTableCommand(srcStateNum, dstStateNum, currentConditions,
                                       currentPktAction, actionsForEfsm, matchStr);
            efsmTableCommands += efsmTableCommand;
        }
    } else if (selectExpression->is<IR::PathExpression>()) {
        std::array<bool, 4 /*MAX_CONDITIONS*/> currentConditions = {false, false, false, false};
        cstring emptyMatch = "0&&&0 ";
        cstring matchStr = "";
        // c’est sketchy, les variables de matchs devraient être déclarées avant l’EFSM et on
        // devrait les récupérer à cet endroit là
        for (size_t i = 0; i < otherMatches.size(); i++) {
            matchStr += emptyMatch;
        }
        cstring dstState = selectExpression->toString();
        int dstStateNum = findPositionInVector(stateStringToNum, dstState);
        cstring actionsForEfsm = formatActionParsed(regActionsParsedVec, operations);
        cstring efsmTableCommand =
            formatEfsmTableCommand(srcStateNum, dstStateNum, currentConditions, currentPktAction,
                                   actionsForEfsm, matchStr);
        efsmTableCommands += efsmTableCommand;
    }
}

const IR::Node *EfsmToFlowBlaze::preorder(IR::P4Efsm *efsm) {
    std::vector<cstring> stateStringToNum;
    std::vector<std::pair<cstring, IR::MethodCallStatement>> calledPktActionsMap;
    std::vector<cstring> globalDataVariables;
    std::vector<cstring> flowDataVariables;
    std::vector<CondPar> conditionsParsedVec;
    std::vector<std::string> otherMatches;

    const std::map<cstring, int> operations = {{"NOP", 0}, {"+", 1},  {"-", 2},
                                               {">>", 3},  {"<<", 4}, {"*", 5}};

    const std::map<cstring, int> registers = {{"meta", 0xF1}, {"now", 0xF2}, {"EXPL", 0xFF}};

    const std::map<cstring, uint> conditions = {{"NOP", 0b000}, {"==", 0b001}, {">=", 0b011},
                                                {"<=", 0b101},  {">", 0b010},  {"<", 0b100}};

    for (const auto *state : efsm->states) {
        stateStringToNum.push_back(state->name.toString());
    }

    cstring efsmTableCommands = "";

    for (const auto *state : efsm->states) {
        std::vector<RegActPar> regActionsParsedVec;
        int currentPktAction = 0;
        // std::array<bool, MAX_CONDITIONS> currentConditions = {false, false, false, false};
        cstring srcState = state->name.toString();
        int srcStateNum = findPositionInVector(stateStringToNum, srcState);

        for (const auto *ligne : state->components) {
            parseAction(ligne, calledPktActionsMap, regActionsParsedVec, globalDataVariables,
                        flowDataVariables, operations, registers, currentPktAction);
        }

        parseTransition(state->selectExpression, conditions, conditionsParsedVec,
                        globalDataVariables, flowDataVariables, regActionsParsedVec,
                        stateStringToNum, operations, registers, otherMatches, efsmTableCommands,
                        currentPktAction, srcStateNum);
    }

    cstring conditionEntry = formatConditionParsed(conditionsParsedVec, conditions);

    cstring pktActionsTableEntries = formatPktActionEntries(calledPktActionsMap);

    std::ofstream o("flowblaze-table-commands.txt");
    o << conditionEntry << std::endl
      << pktActionsTableEntries << std::endl
      << efsmTableCommands << std::endl;
    o.close();

    return nullptr;
}

/*const IR::Node *EfsmToFlowBlaze::preorder(IR::MethodCallExpression *call) {
    if (call->method->is<IR::Member>()) {
        auto *membre = call->method->as<IR::Member>().clone();
        if (strcmp(membre->member.toString(), "apply") == 0) {
            if (strcmp(membre->expr->toString(), "MyEFSM") == 0) {
                membre->expr = new IR::PathExpression(IR::ID("FlowBlaze"));
            }
        }
        auto *newCall = new IR::MethodCallExpression(new IR::Member(*membre));
        return newCall;
    }
    return call;
}
*/
const IR::Node *EfsmToDfaSynthesis::preorder(IR::P4Efsm * /*efsm*/) {
    /*json dfaTotal;
    std::vector<cstring> sigma;

    dfaTotal["initial"] = "start";

    for (const auto *state : efsm->states) {
        dfaTotal["states"].push_back(state->name.toString());
        if (state->selectExpression->is<IR::SelectExpression>()) {
            for (const auto *const sCase :
                 state->selectExpression->as<IR::SelectExpression>().selectCases) {
                cstring transitionSymbol = sCase->keyset->toString();
                if (!sCase->keyset->is<IR::DefaultExpression>()) {
                    if (std::find(sigma.begin(), sigma.end(), transitionSymbol) == sigma.end())
    { sigma.push_back(transitionSymbol); dfaTotal["sigma"].push_back(transitionSymbol);
                    }
                    dfaTotal["transitions"].push_back(std::vector<cstring>{
                        state->name.toString(), transitionSymbol, sCase->state->toString()});
                }
            }
        }
    }

    dfaTotal["accepting"].push_back(dfaTotal["states"].at(dfaTotal["states"].size() - 1));

    std::ofstream o("for-dfa-synthesis.json");
    o << std::setw(4) << dfaTotal << std::endl;
    o.close();*/

    return nullptr;
}

Converter::Converter(ParserOptions::EfsmBackendType efsmBackend)
    : classSettingsMap(new std::map<cstring, ClassSettings>()),
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
    if (!node->is<IR::P4Program>()) {
        BUG("Converter only accepts IR::Globals, not %1%", node);
    }
    return PassManager::init_apply(node);
}

}  // namespace F4
