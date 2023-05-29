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

class RegisterClass : public Transform {
    std::map<cstring, IR::IndexedVector<IR::Declaration>> *laClassMap;

 public:
    explicit RegisterClass(std::map<cstring, IR::IndexedVector<IR::Declaration>> *laClassMap)
        : laClassMap(laClassMap)
        { setName("RegisterClass"); }

    const IR::Node *preorder(IR::P4Class *laclass) override {
        laClassMap->emplace(laclass->name.toString(), laclass->controlLocals);
        return nullptr;
    }
};

class ExtendP4class : public Transform {
    std::map<cstring, IR::IndexedVector<IR::Declaration>> *laClassMap;

 protected:
    static IR::IndexedVector<IR::Declaration> *addName (IR::IndexedVector<IR::Declaration> *ref,
                                                cstring className, cstring instanceName);

 public:
    explicit ExtendP4class(std::map<cstring, IR::IndexedVector<IR::Declaration>> *laClassMap)
        : laClassMap(laClassMap)
        { setName("ExtendP4Class"); }

    const IR::Node *preorder(IR::Declaration_Instance *lobjet) override {
        const cstring typeName = lobjet->type->toString();
        const cstring instanceName = lobjet->Name();
        if (laClassMap->count(typeName) > 0) {
            return addName(&laClassMap->find(typeName)->second, typeName, instanceName);
        }
            return lobjet;
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
};

} // namespace F4

#endif /* _FRONTENDS_P4_FROMO4_CONVERTERS_H_ */
