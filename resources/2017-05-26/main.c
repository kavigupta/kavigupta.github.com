
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include "3sat.h"

int main(int argc, char **argv) {
    if (argc == 1) {
        printf("Input a list of space-separated clauses, each one is a space-separated list of integers\n");
        printf("For example, 1 -2 3 -1 2 4 corresponds to (x_1 OR not x_2 OR x_3)(not x_1 OR x_2 OR x_4)\n");
        return 0;
    }
    assert(argc % 3 == 1);
    int number_clauses = argc / 3;
    instance_3sat_t *inst = malloc(2 * sizeof (unsigned) + number_clauses * sizeof(clause_t));
    inst->num_clauses = number_clauses;
    inst->num_vars = 0;
    literal_t literal(const char *items) {
        int x = atoi(items);
        literal_t lit = {abs(x) - 1, x > 0};
        if (abs(x) > inst->num_vars) inst->num_vars = abs(x);
        return lit;
    }
    for (int i = 0; i < number_clauses; i++) {
        inst->clauses[i].first = literal(argv[i * 3 + 1]);
        inst->clauses[i].second = literal(argv[i * 3 + 2]);
        inst->clauses[i].third = literal(argv[i * 3 + 3]);
    }
    bool result = solve_3SAT(inst);
    printf("The instance was %s\n", result ? "satisfiable" : "unsatisfiable");
}
