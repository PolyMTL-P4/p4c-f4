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
#include <typeindex>
#include <typeinfo>

#include "frontends/p4/coreLibrary.h"
#include "ir/dump.h"
#include "ir/indexed_vector.h"
#include "ir/ir-generated.h"
#include "ir/ir.h"
#include "ir/pass_manager.h"
#include "ir/visitor.h"
#include "lib/safe_vector.h"

namespace F4 {

/// Detects whether there are two declarations in the P4-14 program
/// with the same name and for the same kind of object.
class ChangeName : public Transform {
 public:
    ChangeName() { setName("ChangeName"); }

    const IR::Node *preorder(IR::P4Class *laclass) override {
        //dump(program);
        //auto *tout_en_P4 = new IR::P4Program(program->srcInfo, program->objects);
        std::cout << "p444444444444444444444444" << std::endl;
        //dump(tout_en_P4);
        /*auto toMove = new IR::IndexedVector<IR::StatOrDecl>();
        for (auto e : laclass->body) {
            toMove->push_back();
        }*/
        auto toutEnP4 = new IR::IndexedVector<IR::Declaration>(laclass->controlLocals);
        /*auto stats = new IR::IndexedVector<IR::StatOrDecl>();
        for (auto e : laclass->body->components) {
            stats->push_back(e);
        }
        auto result = new IR::BlockStatement(laclass->body->srcInfo, *stats);
        return result;*/
        //auto toutEnP4 = new IR::P4Action(laclass->srcInfo, laclass->name, laclass->parameters, laclass->body);
        return toutEnP4;
    }
};

///////////////////////////////////////////////////////////////

// Is fed a P4-14 program and outputs an equivalent P4-16 program in v1model
class Converter : public PassManager {
 public:
    Converter();
    void loadModel() {}
    Visitor::profile_t init_apply(const IR::Node *node) override;
};

} // namespace F4

#endif /* _FRONTENDS_P4_FROMO4_CONVERTERS_H_ */
