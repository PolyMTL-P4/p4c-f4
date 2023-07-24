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

IR::IndexedVector<IR::Declaration> *ExtendP4class::addNameToDecls (IR::IndexedVector<IR::Declaration> *ref,
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

Converter::Converter(ParserOptions::EfsmBackendType efsmBackend) : laClassMap(new std::map<cstring, IR::IndexedVector<IR::Declaration>>()),
                         laParaMap(new std::map<cstring, IR::ParameterList>()),
                         instanceMap(new std::map<cstring, cstring>()) {
    setName("Converter");

    passes.emplace_back(new RegisterClass(laClassMap, laParaMap));
    passes.emplace_back(new ExtendP4class(laClassMap, laParaMap, instanceMap));
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
