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

#ifndef _FRONTENDS_P4_FROMF4_CONVERTERS_H_
#define _FRONTENDS_P4_FROMF4_CONVERTERS_H_

#include <iostream>
#include <map>
#include <typeindex>
#include <typeinfo>
#include <utility>

#include "frontends/p4/coreLibrary.h"
#include "ir/dump.h"
#include "ir/indexed_vector.h"
#include "ir/ir-generated.h"
#include "ir/ir.h"
#include "ir/pass_manager.h"
#include "ir/visitor.h"
#include "lib/cstring.h"
#include "lib/safe_vector.h"

namespace F4 {

struct ClassSettings{
    IR::IndexedVector<IR::Declaration> decls;
    IR::ParameterList                  params;
};

class ReplaceMembers : public Transform {
    std::map<cstring, cstring> *instanceToClassNameMap;

 public:
    explicit ReplaceMembers(std::map<cstring, cstring> *instanceToClassNameMap) : instanceToClassNameMap(instanceToClassNameMap) {
        setName("ReplaceMembers");
    }

    const IR::Node *preorder(IR::Member *IRmember) override {
        auto exprName = IRmember->expr->toString();
        auto memberName = IRmember->member.toString();
        if (auto found = instanceToClassNameMap->find(exprName); found != instanceToClassNameMap->end()) {
            auto finalName = memberName + "_" + found->second + "_" + exprName;
            return new IR::PathExpression(IR::ID(finalName));
        }
        return IRmember;
    }
};


class ReplaceParameters : public Transform {
    std::map<cstring, IR::Argument> *paramArgMap;
    std::map<cstring, cstring> *substituteVars;

 public:
    explicit ReplaceParameters(std::map<cstring, IR::Argument> *paramArgMap, std::map<cstring, cstring> *substituteVars) : paramArgMap(paramArgMap), substituteVars(substituteVars)
    { setName("ReplaceParameters"); }

    const IR::Node *postorder(IR::Expression *expr) override {
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
};

class RegisterClass : public Transform {
    std::map<cstring, ClassSettings> *classSettingsMap;

 public:
    explicit RegisterClass(std::map<cstring, ClassSettings> *classSettingsMap)
        : classSettingsMap(classSettingsMap)
        { setName("RegisterClass"); }

    const IR::Node *preorder(IR::P4Class *laClass) override {
        ClassSettings settings; 
        settings.decls = laClass->localDeclarations;
        settings.params = *(laClass->getParameters());
        classSettingsMap->emplace(laClass->name.toString(), settings);
        return nullptr;
    }
};

class ExtendP4class : public Transform {
    std::map<cstring, ClassSettings> *classSettingsMap;
    std::map<cstring, cstring> *instanceToClassNameMap;

 protected:
    static IR::IndexedVector<IR::Declaration> *addNameToDecls (IR::IndexedVector<IR::Declaration> *ref,
                                                cstring className, cstring instanceName,
                                                std::map<cstring, cstring> *substituteVars);

 public:
    explicit ExtendP4class(std::map<cstring, ClassSettings> *classSettingsMap,
                           std::map<cstring, cstring> *instanceToClassNameMap)
        : classSettingsMap(classSettingsMap), instanceToClassNameMap(instanceToClassNameMap)
        { setName("ExtendP4Class"); }

    const IR::Node *preorder(IR::Declaration_Instance *object) override {
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

    const IR::Node *preorder(IR::Expression *call) override {
        const auto *newCall = call->clone();
        newCall = newCall->apply(ReplaceMembers(instanceToClassNameMap));
        return newCall;
    }
};

///////////////////////////////////////////////////////////////

// Is fed a F4 program and outputs an equivalent P4-16 program
class Converter : public PassManager {
 public:
    Converter();
    void loadModel() {}
    Visitor::profile_t init_apply(const IR::Node *node) override;
    
 private:
    std::map<cstring, ClassSettings> *classSettingsMap;
    std::map<cstring, cstring> *instanceToClassNameMap;
};

} // namespace F4

#endif /* _FRONTENDS_P4_FROMO4_CONVERTERS_H_ */
