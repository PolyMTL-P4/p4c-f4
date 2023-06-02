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

class ReplaceParameters : public Transform {
    std::map<cstring, IR::Argument> *paramArgMap;
    std::map<cstring, cstring> *substituteVars;

 public:
    explicit ReplaceParameters(std::map<cstring, IR::Argument> *paramArgMap, std::map<cstring, cstring> *substituteVars) : paramArgMap(paramArgMap), substituteVars(substituteVars)
    { setName("ReplaceParameters"); }

    const IR::Node *postorder(IR::Expression *lExpr) override {
        if (paramArgMap->count(lExpr->toString()) > 0) {
            const auto *newExpr = paramArgMap->find(lExpr->toString())->second.expression;
            return newExpr;
        }
        if (substituteVars->count(lExpr->toString()) > 0) {
            auto *newPath = new IR::PathExpression(IR::ID(substituteVars->find(lExpr->toString())->second));
            return newPath;
        }
        return lExpr;
    }
};

class RegisterClass : public Transform {
    std::map<cstring, IR::IndexedVector<IR::Declaration>> *laClassMap;
    std::map<cstring, IR::ParameterList> *laParaMap;

 public:
    explicit RegisterClass(std::map<cstring, IR::IndexedVector<IR::Declaration>> *laClassMap,
                           std::map<cstring, IR::ParameterList> *laParaMap)
        : laClassMap(laClassMap), laParaMap(laParaMap)
        { setName("RegisterClass"); }

    const IR::Node *preorder(IR::P4Class *laclass) override {
        laClassMap->emplace(laclass->name.toString(), laclass->localDeclarations);
        laParaMap->emplace(laclass->name.toString(), *(laclass->getParameters()));
        return nullptr;
    }
};

class ExtendP4class : public Transform {
    std::map<cstring, IR::IndexedVector<IR::Declaration>> *laClassMap;
    std::map<cstring, IR::ParameterList> *laParaMap;
    std::map<cstring, cstring> *instanceMap;
    std::map<cstring, IR::Argument> *paramArgMap;
    std::map<cstring, cstring> *substituteVars;

 protected:
    static IR::IndexedVector<IR::Declaration> *addNameToDecls (IR::IndexedVector<IR::Declaration> *ref,
                                                cstring className, cstring instanceName,
                                                std::map<cstring, cstring> *substituteVars);

 public:
    explicit ExtendP4class(std::map<cstring, IR::IndexedVector<IR::Declaration>> *laClassMap,
                           std::map<cstring, IR::ParameterList> *laParaMap,
                           std::map<cstring, cstring> *instanceMap)
        : laClassMap(laClassMap), laParaMap(laParaMap), instanceMap(instanceMap)
        { setName("ExtendP4Class"); }

    const IR::Node *preorder(IR::Declaration_Instance *lobjet) override {
        const cstring className = lobjet->type->toString();
        const cstring instanceName = lobjet->Name();
        
        auto *paramArgMap = new std::map<cstring, IR::Argument>();
        auto *substituteVars =  new std::map<cstring, cstring>();
        if (laClassMap->count(className) > 0) {
            auto lesParams = laParaMap->find(className)->second.parameters;
            //auto chepakoi = new std::map<cstring, IR::Argument>;
            for (size_t i = 0; i < lesParams.size(); i++) {
                paramArgMap->emplace(lesParams.at(i)->name.toString(), *(lobjet->arguments->at(i)));
            }
            instanceMap->emplace(instanceName, className);
            auto *renamedDecls = addNameToDecls(&laClassMap->find(className)->second, className, instanceName, substituteVars);
            //auto visiteur = new ReplaceParameters(paramArgMap);
            const auto *result = renamedDecls->apply(ReplaceParameters(paramArgMap, substituteVars));
            return result;
        }
            return lobjet;
    }

    const IR::Node *preorder(IR::MethodCallExpression *lecall) override {
        if (lecall->method->is<IR::Member>()) {
            const auto *lemembre = lecall->method->to<IR::Member>();
            auto exprName = lemembre->expr->toString();
            auto instName = lemembre->member.toString();
            if (instanceMap->count(exprName) > 0) {
                auto finalName = instName + "_" + instanceMap->find(exprName)->second + "_" + exprName;
                return new IR::MethodCallExpression(lecall->srcInfo, new IR::PathExpression(IR::ID(finalName)));
            }
        }
        return lecall;
    }
};

///////////////////////////////////////////////////////////////

// Is fed a F4 program and outputs an equivalent P4-16 program
class Converter : public PassManager {
 public:
    Converter();
    void loadModel() {}
    Visitor::profile_t init_apply(const IR::Node *node) override;
    std::map<cstring, IR::IndexedVector<IR::Declaration>> *laClassMap;
    std::map<cstring, IR::ParameterList> *laParaMap;
    std::map<cstring, cstring> *instanceMap;
};

} // namespace F4

#endif /* _FRONTENDS_P4_FROMO4_CONVERTERS_H_ */
