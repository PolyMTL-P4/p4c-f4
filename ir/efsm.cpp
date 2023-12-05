#include "ir/ir-generated.h"
namespace IR {

void P4Efsm::checkDuplicates() const {
    for (auto decl : states) {
        auto prev = efsmLocals.getDeclaration(decl->getName().name);
        if (prev != nullptr)
            ::error(ErrorType::ERR_DUPLICATE, "State %1% has same name as %2%", decl, prev);
    }
}

void P4Dfa::checkDuplicates() const {
    for (auto decl : states) {
        auto prev = efsmLocals.getDeclaration(decl->getName().name);
        if (prev != nullptr)
            ::error(ErrorType::ERR_DUPLICATE, "State %1% has same name as %2%", decl, prev);
    }
}

}