#include <iostream>
#include "yaml-cpp/yaml.h"

void yamltest( void ) {
    YAML::Emitter out;
    out << "Hello, World!";
   
    std::cout << "Here's the output YAML:\n" << out.c_str(); // prints "Hello, World!"
}


using namespace std;


#ifdef __cplusplus
extern "C" {
#endif 

void testffi( void )
{
  cout << "test c++ ffi" << endl;
  yamltest();
}

#ifdef __cplusplus
}
#endif
