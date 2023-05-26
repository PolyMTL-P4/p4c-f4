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
#include "frontends/common/resolveReferences/referenceMap.h"
#include "frontends/common/resolveReferences/resolveReferences.h"
#include "frontends/p4/coreLibrary.h"
#include "ir/indexed_vector.h"
#include "ir/ir-generated.h"
#include "lib/big_int_util.h"

namespace F4 {

///////////////////////////////////////////////////////////////

Converter::Converter() : laClassMap(new IR::IndexedVector<IR::Declaration>()) {
    setName("Converter");

    passes.emplace_back(new RegisterClass(laClassMap));
    passes.emplace_back(new ExtendP4class(laClassMap));
}

Visitor::profile_t Converter::init_apply(const IR::Node *node) {
    if (!node->is<IR::P4Program>()) { BUG("Converter only accepts IR::Globals, not %1%", node);
}
    return PassManager::init_apply(node);
}

} // namespace F4
