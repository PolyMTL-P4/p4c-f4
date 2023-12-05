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

#include "frontends/common/parser_options.h"
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

struct ClassSettings {
    IR::IndexedVector<IR::Declaration> decls;
    IR::ParameterList params;
};

class ReplaceMembers : public Transform {
    std::map<cstring, cstring> *instanceToClassNameMap;

 public:
    explicit ReplaceMembers(std::map<cstring, cstring> *instanceToClassNameMap)
        : instanceToClassNameMap(instanceToClassNameMap) {
        setName("ReplaceMembers");
    }

    const IR::Node *preorder(IR::Member *IRmember) override;
};

class ReplaceParameters : public Transform {
    std::map<cstring, IR::Argument> *paramArgMap;
    std::map<cstring, cstring> *substituteVars;

 public:
    explicit ReplaceParameters(std::map<cstring, IR::Argument> *paramArgMap,
                               std::map<cstring, cstring> *substituteVars)
        : paramArgMap(paramArgMap), substituteVars(substituteVars) {
        setName("ReplaceParameters");
    }

    const IR::Node *postorder(IR::Expression *expr) override;
};

class RegisterClass : public Transform {
    std::map<cstring, ClassSettings> *classSettingsMap;

 public:
    explicit RegisterClass(std::map<cstring, ClassSettings> *classSettingsMap)
        : classSettingsMap(classSettingsMap) {
        setName("RegisterClass");
    }

    const IR::Node *preorder(IR::P4Class *laClass) override;
};

class ExtendP4Class : public Transform {
    std::map<cstring, ClassSettings> *classSettingsMap;
    std::map<cstring, cstring> *instanceToClassNameMap;

 protected:
    static IR::IndexedVector<IR::Declaration> *addNameToDecls(
        IR::IndexedVector<IR::Declaration> *ref, cstring className, cstring instanceName,
        std::map<cstring, cstring> *substituteVars);

 public:
    explicit ExtendP4Class(std::map<cstring, ClassSettings> *classSettingsMap,
                           std::map<cstring, cstring> *instanceToClassNameMap)
        : classSettingsMap(classSettingsMap), instanceToClassNameMap(instanceToClassNameMap) {
        setName("ExtendP4Class");
    }

    const IR::Node *preorder(IR::Declaration_Instance *object) override;

    const IR::Node *preorder(IR::Expression *call) override;
};

class DiscoverExpression : public Inspector {
    bool preorder(const IR::Expression *expr) override {
        std::cout << expr->toString() << std::endl;
        return true;
    }
};

class EfsmToFlowBlaze : public Transform {
    // Generate commands to populate tables
 public:
    explicit EfsmToFlowBlaze() { setName("EfsmToFlowBlaze"); }

    const IR::Node *preorder(IR::P4Efsm *efsm) override;
    /*const IR::Node *preorder(IR::MethodCallExpression *call) override;*/
};

class EfsmToDfaSynthesis : public Transform {
    // Generate commands to populate tables
 public:
    explicit EfsmToDfaSynthesis() { setName("EfsmToDfaSynthesis"); }

    const IR::Node *preorder(IR::P4Dfa *dfa) override;
};

///////////////////////////////////////////////////////////////

// Is fed a F4 program and outputs an equivalent P4-16 program
class Converter : public PassManager {
 public:
    explicit Converter();
    void loadModel() {}
    Visitor::profile_t init_apply(const IR::Node *node) override;

 private:
    std::map<cstring, ClassSettings> *classSettingsMap;
    std::map<cstring, cstring> *instanceToClassNameMap;
};

}  // namespace F4

#endif /* _FRONTENDS_P4_FROMO4_CONVERTERS_H_ */
