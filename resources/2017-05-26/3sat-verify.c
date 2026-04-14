#include "3sat-verify.h"

bool verify_3SAT(instance_3sat_t *inst, bool *soln) {
    for (int i = 0; i < inst->num_clauses; i++) {
        clause_t c = inst->clauses[i];
        bool clause_satisfiable = c.first.not_negated == soln[c.first.name];
        clause_satisfiable |= c.second.not_negated == soln[c.second.name];
        clause_satisfiable |= c.third.not_negated == soln[c.third.name];
        if (!clause_satisfiable) {
            return false;
        }
    }
    return true;
}
