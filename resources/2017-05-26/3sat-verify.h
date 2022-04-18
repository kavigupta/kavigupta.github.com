#include <stdbool.h>
typedef unsigned variable_t;
typedef struct { variable_t name; bool not_negated; } literal_t;
typedef struct { literal_t first, second, third; } clause_t;
typedef struct { unsigned num_vars, num_clauses; clause_t clauses[]; } instance_3sat_t;
bool verify_3SAT(instance_3sat_t *inst, bool *soln);
