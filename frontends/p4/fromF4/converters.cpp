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
#include <cstddef>
#include <fstream>
#include <regex>
#include <sstream>
#include <string>

enum {
    MAX_REG_ACTIONS_PER_TRANSITION = 3,
    MAX_CONDITIONS = 4,
    FDV_BASE_REGISTER = 0x00,
    GDV_BASE_REGISTER = 0x0F,
    REVERSE_CONDITION_XOR = 0b111,
    SWAP_CONDITION_XOR = 0b110
};

struct RegActPar {
    std::string resultOp;
    cstring operation;
    std::string operationStr;
    std::string leftOp;
    std::string rightOp;
};

struct CondPar {
    uint cond;
    std::string condStr;
    std::string leftOp;
    std::string rightOp;
    std::string matchVariable;
    bool operator==(const CondPar &laCond) const {
        return ((cond == laCond.cond) && (condStr == laCond.condStr) && (leftOp == laCond.leftOp) &&
                (rightOp == laCond.rightOp) && (matchVariable == laCond.matchVariable)) ||
               (((cond ^ SWAP_CONDITION_XOR) == laCond.cond) && (condStr == laCond.condStr) &&
                (leftOp == laCond.leftOp) && (rightOp == laCond.rightOp) &&
                (matchVariable == laCond.matchVariable));
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
int findPositionInVector(std::vector<T> &vector, T &element) {
    int elemPos = 0;
    auto elemFound = std::find(vector.begin(), vector.end(), element);
    if (elemFound != vector.end()) {
        elemPos = elemFound - vector.begin();
    } else {
        BUG("Element not found\n");
    }
    return elemPos;
}

template <typename T>
void insertIfNotInVec(std::vector<T> &vector, T element) {
    if (std::find(vector.begin(), vector.end(), element) == vector.end()) {
        vector.push_back(element);
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

void fillActionOrConditionParsed(const IR::Expression *expr, std::string &sideOp,
                                 std::vector<cstring> &globalDataVariables,
                                 std::vector<cstring> &flowDataVariables) {
    cstring laStr = expr->toString();
    if (expr->is<IR::Constant>()) {
        sideOp = laStr;
    } else if (strncmp(laStr, "#", 1) == 0) {
        insertIfNotInVec(globalDataVariables, laStr);
        uint elemPos = findPositionInVector(globalDataVariables, laStr);
        sideOp = "flowblaze_metadata.G" + std::to_string(elemPos);
    } else if (strcmp(laStr, "now") == 0) {
        sideOp = "(bit<32>)standard_metadata.ingress_global_timestamp";
    } else if (strcmp(laStr, "meta") == 0) {
        sideOp = "flowblaze_metadata.pkt_data";
    } else {
        insertIfNotInVec(flowDataVariables, laStr);
        int elemPos = findPositionInVector(flowDataVariables, laStr);
        sideOp = "flowblaze_metadata.R" + std::to_string(elemPos);
    }
}

void parseRegAction(RegActPar &actionParsed, const IR::AssignmentStatement *varAss,
                    std::vector<cstring> &globalDataVariables,
                    std::vector<cstring> &flowDataVariables,
                    const std::map<cstring, int> &operations) {
    cstring res = varAss->left->toString();
    if (strncmp(res, "#", 1) == 0) {
        insertIfNotInVec(globalDataVariables, res);
        uint elemPos = findPositionInVector(globalDataVariables, res);
        actionParsed.resultOp =
            "reg_G" + std::to_string(elemPos) + ".write(" + std::to_string(elemPos) + ", ";
    } else {  // gérer les erreurs également
        insertIfNotInVec(flowDataVariables, res);
        int elemPos = findPositionInVector(flowDataVariables, res);
        actionParsed.resultOp =
            "reg_R" + std::to_string(elemPos) + ".write(flowblaze_metadata.update_state_index, ";
    }
    if (varAss->right->is<IR::Operation_Binary>()) {
        const auto *opTotal = varAss->right->to<IR::Operation_Binary>();
        fillActionOrConditionParsed(opTotal->left, actionParsed.leftOp, globalDataVariables,
                                    flowDataVariables);
        actionParsed.operation = intToHexStr(operations.at(opTotal->getStringOp()));
        actionParsed.operationStr = opTotal->getStringOp();
        fillActionOrConditionParsed(opTotal->right, actionParsed.rightOp, globalDataVariables,
                                    flowDataVariables);
    } else {
        fillActionOrConditionParsed(varAss->right, actionParsed.leftOp, globalDataVariables,
                                    flowDataVariables);
        actionParsed.operation = intToHexStr(operations.at("+"));
        actionParsed.operationStr = "+";
        actionParsed.rightOp = "0";
    }
}

void parseStatement(const IR::StatOrDecl *line,
                    std::vector<std::pair<cstring, IR::MethodCallStatement>> &calledPktActionsMap,
                    std::vector<RegActPar> &regActionsParsed,
                    std::vector<cstring> &globalDataVariables,
                    std::vector<cstring> &flowDataVariables,
                    const std::map<cstring, int> &operations, int &currentPktAction) {
    if (line->is<IR::MethodCallStatement>()) {
        parseActionCall(line->to<IR::MethodCallStatement>(), calledPktActionsMap, currentPktAction);
    } else if (line->is<IR::AssignmentStatement>()) {
        RegActPar actionParsed = {"", "0", "", "", ""};
        parseRegAction(actionParsed, line->to<IR::AssignmentStatement>(), globalDataVariables,
                       flowDataVariables, operations);
        regActionsParsed.emplace_back(actionParsed);
    } else {
        cstring errorMsg = "Non identified statement: " + line->toString();
        BUG(errorMsg);
    }
}

void parseConditionOrMatch(const IR::SelectExpression &selectExpr, std::vector<CondPar> &selectArgs,
                           const std::map<cstring, uint> &conditions,
                           std::vector<cstring> &globalDataVariables,
                           std::vector<cstring> &flowDataVariables) {
    for (const auto *conditionOrMatch : selectExpr.select->components) {
        CondPar conditionParsed = {0, "", "", "", ""};
        if (conditionOrMatch->is<IR::Operation_Relation>()) {
            const auto *cond = conditionOrMatch->to<IR::Operation_Relation>();
            conditionParsed.cond = conditions.at(cond->getStringOp());
            conditionParsed.condStr = cond->getStringOp();
            fillActionOrConditionParsed(cond->left, conditionParsed.leftOp, globalDataVariables,
                                        flowDataVariables);
            fillActionOrConditionParsed(cond->right, conditionParsed.rightOp, globalDataVariables,
                                        flowDataVariables);
        } else {
            conditionParsed.matchVariable = conditionOrMatch->toString();
        }
        selectArgs.push_back(conditionParsed);
    }
}

void insertCondition(const CondPar &conditionTrue, const std::map<cstring, uint> &conditions,
                     std::vector<CondPar> &conditionsParsedVec,
                     std::array<bool, 4> &currentConditions, bool reverseCondition = false) {
    uint cond = reverseCondition ? conditionTrue.cond ^ REVERSE_CONDITION_XOR : conditionTrue.cond;
    std::string condStr;
    for (auto condition : conditions) {
        if (condition.second == cond) {
            condStr = condition.first;
        }
    }
    CondPar conditionParsed = {cond, condStr, conditionTrue.leftOp, conditionTrue.rightOp,
                               conditionTrue.matchVariable};
    insertIfNotInVec(conditionsParsedVec, conditionParsed);
    currentConditions.at(findPositionInVector(conditionsParsedVec, conditionParsed)) = true;
}

void parseKeyset(const IR::Expression *expr, CondPar &selectArg,
                 const std::map<cstring, uint> &conditions,
                 std::vector<CondPar> &conditionsParsedVec, std::array<bool, 4> &currentConditions,
                 std::vector<std::string> &otherMatches, cstring &matchStr) {
    bool defaultCase = expr->is<IR::DefaultExpression>();
    cstring transitionSymbol = expr->toString();
    if (selectArg.matchVariable.empty()) {
        if (!defaultCase) {
            if (strcmp(transitionSymbol, "true") == 0) {
                insertCondition(selectArg, conditions, conditionsParsedVec, currentConditions,
                                false);
            } else if (strcmp(transitionSymbol, "false") == 0) {
                insertCondition(selectArg, conditions, conditionsParsedVec, currentConditions,
                                true);
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

std::string formatActionCode(int &srcStateNum, std::vector<RegActPar> &regActionsParsedVec) {
    // TODO(florent): handle else
    std::string actionCode =
        "        if (flowblaze_metadata.state == " + std::to_string(srcStateNum) + ") {\n";
    for (const auto &regActionParsed : regActionsParsedVec) {
        actionCode += "            t_result = " + regActionParsed.leftOp + " " +
                      regActionParsed.operationStr + " " + regActionParsed.rightOp + ";\n";
        actionCode += "            " + regActionParsed.resultOp + "t_result);\n";
    }
    actionCode += "        }\n";
    return actionCode;
}

cstring formatPktActionsCommands(
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

cstring formatEfsmTableCommand(int &srcStateNum, int &currentPktAction) {
    cstring efsmTableCommand = "table_add FlowBlaze.EFSM_table define_operation_update_state ";
    efsmTableCommand += std::to_string(srcStateNum) + "&&&0xFFFF " + " => " +
                        std::to_string(currentPktAction) + " 1"  // priority
                        + "\n";
    return (efsmTableCommand);
}

std::string formatTransitionCode(int &srcStateNum, int &dstStateNum,
                                 std::vector<CondPar> &conditionsParsedVec,
                                 std::array<bool, 4> &currentConditions, cstring &otherMatch) {
    // il faut gérer les else correctement
    std::string dstStateWrite =
        (std::ostringstream() << "reg_state.write(flowblaze_metadata.update_state_index, (bit<16>)"
                              << dstStateNum << ");")
            .str();
    std::string conditionsIfs = dstStateWrite;
    for (size_t i = 0; i < currentConditions.size(); i++) {
        if (currentConditions.at(i)) {
            CondPar condition = conditionsParsedVec.at(i);
            conditionsIfs =
                (std::ostringstream()
                 << "if (" << condition.leftOp << condition.condStr << condition.rightOp
                 << ") {\n    " << std::regex_replace(conditionsIfs, std::regex("\n"), "\n    ")
                 << "\n}")
                    .str();
        }
    }
    auto transitionCode = std::ostringstream()
                          << "\nif (flowblaze_metadata.state == " << srcStateNum << ") {\n"
                          << std::regex_replace(conditionsIfs, std::regex("\n"), "\n    ") << "\n}";

    return transitionCode.str();
}

void parseTransition(const IR::Expression *selectExpression,
                     const std::map<cstring, uint> &conditions,
                     std::vector<CondPar> &conditionsParsedVec,
                     std::vector<cstring> &globalDataVariables,
                     std::vector<cstring> &flowDataVariables, std::vector<cstring> stateStringToNum,
                     std::vector<std::string> &otherMatches, std::string &transitionCodes,
                     int &srcStateNum) {
    if (selectExpression->is<IR::SelectExpression>()) {
        auto selectExpr = selectExpression->as<IR::SelectExpression>();
        std::vector<CondPar> selectArgs;

        parseConditionOrMatch(selectExpr, selectArgs, conditions, globalDataVariables,
                              flowDataVariables);

        for (const auto *sCase : selectExpr.selectCases) {
            cstring matchStr = "";
            std::array<bool, 4 /*MAX_CONDITIONS*/> currentConditionsResults = {false, false, false,
                                                                               false};
            // cstring transitionSymbol = sCase->keyset->toString();
            if (sCase->keyset->is<IR::ListExpression>()) {
                auto lesExpr = sCase->keyset->as<IR::ListExpression>();
                int i = 0;
                for (const auto *expr : lesExpr.components) {
                    parseKeyset(expr, selectArgs.at(i), conditions, conditionsParsedVec,
                                currentConditionsResults, otherMatches, matchStr);
                    i++;
                }
            } else {
                parseKeyset(sCase->keyset, selectArgs.at(0), conditions, conditionsParsedVec,
                            currentConditionsResults, otherMatches, matchStr);
            }
            cstring dstState = sCase->state->toString();
            int dstStateNum = findPositionInVector(stateStringToNum, dstState);
            std::string transitionCode = formatTransitionCode(
                srcStateNum, dstStateNum, conditionsParsedVec, currentConditionsResults, matchStr);
            transitionCodes += std::regex_replace(transitionCode, std::regex("\n"), "\n        ");
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
        std::string transitionCode = formatTransitionCode(
            srcStateNum, dstStateNum, conditionsParsedVec, currentConditions, matchStr);
        transitionCodes += std::regex_replace(transitionCode, std::regex("\n"), "\n        ");
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

    const std::map<cstring, uint> conditions = {{"NOP", 0b000}, {"==", 0b001}, {">=", 0b011},
                                                {"<=", 0b101},  {">", 0b010},  {"<", 0b100}};

    for (const auto *state : efsm->states) {
        stateStringToNum.push_back(state->name.toString());
    }

    cstring efsmTableCommands = "";
    std::string actionsCode =
        R"(// ----------------------- UPDATE LOGIC BLOCK ----------------------------------
control UpdateLogic(inout HEADER_NAME hdr,
                    inout flowblaze_t flowblaze_metadata,
                    in standard_metadata_t standard_metadata) {

    apply{
        // Calculate update lookup index
        // TODO: (improvement) save hash in metadata when calculated for reading registers
        hash(flowblaze_metadata.update_state_index,
             HashAlgorithm.crc32,
             (bit<32>) 0,
             FLOW_SCOPE,
             (bit<32>) CONTEXT_TABLE_SIZE);

        bit<32> t_result = 0;
)";
    std::string transitionCode =
        R"(// ----------------------- UPDATE STATE BLOCK ----------------------------------
control UpdateState(inout HEADER_NAME hdr,
                    inout flowblaze_t flowblaze_metadata,
                    in standard_metadata_t standard_metadata) {

    apply{
        // Calculate update lookup index
        // TODO: (improvement) save hash in metadata when calculated for reading registers
        hash(flowblaze_metadata.update_state_index,
             HashAlgorithm.crc32,
             (bit<32>) 0,
             FLOW_SCOPE,
             (bit<32>) CONTEXT_TABLE_SIZE);)";

    for (const auto *state : efsm->states) {
        std::vector<RegActPar> regActionsParsedVec;
        int currentPktAction = 0;
        // std::array<bool, MAX_CONDITIONS> currentConditions = {false, false, false, false};
        cstring srcState = state->name.toString();
        int srcStateNum = findPositionInVector(stateStringToNum, srcState);

        for (const auto *line : state->components) {
            parseStatement(line, calledPktActionsMap, regActionsParsedVec, globalDataVariables,
                           flowDataVariables, operations, currentPktAction);
        }

        cstring efsmTableCommand = formatEfsmTableCommand(srcStateNum, currentPktAction);
        efsmTableCommands += efsmTableCommand;
        actionsCode += formatActionCode(srcStateNum, regActionsParsedVec);

        parseTransition(state->selectExpression, conditions, conditionsParsedVec,
                        globalDataVariables, flowDataVariables, stateStringToNum, otherMatches,
                        transitionCode, srcStateNum);
    }

    cstring pktActionsCommands = formatPktActionsCommands(calledPktActionsMap);

    std::ofstream o("flowblaze-table-commands.txt");
    o << pktActionsCommands << std::endl << efsmTableCommands << std::endl;
    o.close();

    actionsCode += "\n    }\n}\n";
    transitionCode += "\n    }\n}\n";
    std::ofstream olib("efsm-lib-content.p4");
    olib << actionsCode << std::endl << transitionCode << std::endl;
    olib.close();

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

Converter::Converter()
    : classSettingsMap(new std::map<cstring, ClassSettings>()),
      instanceToClassNameMap(new std::map<cstring, cstring>()) {
    setName("Converter");

    passes.emplace_back(new RegisterClass(classSettingsMap));
    passes.emplace_back(new ExtendP4Class(classSettingsMap, instanceToClassNameMap));
    passes.emplace_back(new EfsmToFlowBlaze());
    passes.emplace_back(new EfsmToDfaSynthesis());
}

Visitor::profile_t Converter::init_apply(const IR::Node *node) {
    if (!node->is<IR::P4Program>()) {
        BUG("Converter only accepts IR::Globals, not %1%", node);
    }
    return PassManager::init_apply(node);
}

}  // namespace F4
