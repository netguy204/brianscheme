#include "interp.h"
#include "read.h"

int main(int argc, char ** argv) {
  init();
  while(true) {
    object *input = read(stdin);
    print_obj(input);
    print_obj(interp(input, the_global_environment));
  }
  return 0;
}
