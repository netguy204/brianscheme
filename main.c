#include "interp.h"
#include "read.h"
#include "gc.h"

int main(int argc, char ** argv) {
  init();
  while(true) {
    object *input = read(stdin);
    push_root(&input);
    print_obj(interp(input, the_global_environment));
    pop_root(&input);
  }
  return 0;
}
