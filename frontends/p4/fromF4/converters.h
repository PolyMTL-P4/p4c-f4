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
    std::map<cstring, cstring> *instanceMap;

 public:
    explicit ReplaceMembers(std::map<cstring, cstring> *instanceMap) : instanceMap(instanceMap) {
        setName("ReplaceMembers");
    }

    const IR::Node *preorder(IR::Member *elMembre) override {
        auto exprName = elMembre->expr->toString();
        auto memberName = elMembre->member.toString();
        if (auto found = instanceMap->find(exprName); found != instanceMap->end()) {
            auto finalName = memberName + "_" + found->second + "_" + exprName;
            return new IR::PathExpression(IR::ID(finalName));
        }
        return elMembre;
    }
};


class ReplaceParameters : public Transform {
    std::map<cstring, IR::Argument> *paramArgMap;
    std::map<cstring, cstring> *substituteVars;

 public:
    explicit ReplaceParameters(std::map<cstring, IR::Argument> *paramArgMap, std::map<cstring, cstring> *substituteVars) : paramArgMap(paramArgMap), substituteVars(substituteVars)
    { setName("ReplaceParameters"); }

    const IR::Node *postorder(IR::Expression *lExpr) override {
        if (auto arg = paramArgMap->find(lExpr->toString()); arg != paramArgMap->end()) {
            const auto *newExpr = arg->second.expression;
            return newExpr;
        }
        if (auto newName = substituteVars->find(lExpr->toString()); newName != substituteVars->end()) {
            auto *newPath = new IR::PathExpression(IR::ID(newName->second));
            return newPath;
        }
        return lExpr;
    }
};

class RegisterClass : public Transform {
    std::map<cstring, ClassSettings> *laClassMap;

 public:
    explicit RegisterClass(std::map<cstring, ClassSettings> *laClassMap)
        : laClassMap(laClassMap)
        { setName("RegisterClass"); }

    const IR::Node *preorder(IR::P4Class *laclass) override {
        ClassSettings settings; 
        settings.decls = laclass->localDeclarations;
        settings.params = *(laclass->getParameters());
        laClassMap->emplace(laclass->name.toString(), settings);
        return nullptr;
    }
};

class ExtendP4class : public Transform {
    std::map<cstring, ClassSettings> *laClassMap;
    std::map<cstring, cstring> *instanceMap;

 protected:
    static IR::IndexedVector<IR::Declaration> *addNameToDecls (IR::IndexedVector<IR::Declaration> *ref,
                                                cstring className, cstring instanceName,
                                                std::map<cstring, cstring> *substituteVars);

 public:
    explicit ExtendP4class(std::map<cstring, ClassSettings> *laClassMap,
                           std::map<cstring, cstring> *instanceMap)
        : laClassMap(laClassMap), instanceMap(instanceMap)
        { setName("ExtendP4Class"); }

    const IR::Node *preorder(IR::Declaration_Instance *lobjet) override {
        const cstring className = lobjet->type->toString();
        const cstring instanceName = lobjet->Name();
        std::map<cstring, IR::Argument> paramArgMap;
        std::map<cstring, cstring> substituteVars;

        if (auto settingsFound = laClassMap->find(className); settingsFound != laClassMap->end()) {
            ClassSettings settings = settingsFound->second;
            auto lesParams = settings.params.parameters;
            for (size_t i = 0; i < lesParams.size(); i++) {
                paramArgMap.emplace(lesParams.at(i)->name.toString(), *(lobjet->arguments->at(i)));
            }
            instanceMap->emplace(instanceName, className);
            auto *renamedDecls = addNameToDecls(&settings.decls, className, instanceName, &substituteVars);
            const auto *result = renamedDecls->apply(ReplaceParameters(&paramArgMap, &substituteVars));
            return result;
        }
            return lobjet;
    }

    const IR::Node *preorder(IR::Expression *lecall) override {
        const auto *newCall = lecall->clone();
        newCall = newCall->apply(ReplaceMembers(instanceMap));
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
    std::map<cstring, ClassSettings> *laClassMap;
    std::map<cstring, cstring> *instanceMap;
};

} // namespace F4

#endif /* _FRONTENDS_P4_FROMO4_CONVERTERS_H_ */
