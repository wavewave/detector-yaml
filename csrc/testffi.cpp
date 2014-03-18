#include <iostream>
#include <fstream>
#include "yaml-cpp/yaml.h"

void yamlemittest( void ) {
    YAML::Emitter out;
    out << "Hello, World!";
   
    std::cout << "Here's the output YAML:\n" << out.c_str(); // prints "Hello, World!"
}

void yamlparsetest( void ) {
  std::ifstream input("test.yaml");  
  YAML::Node doc = YAML::Load(input);
  std::cout << doc << "\n";


  /*  YAML::Parser parser(fin);

  YAML::Node doc;
  parser.GetNextDocument(doc); 
  std::string scalar; 
  doc >> scalar; 
  std::cout << "That scalar was: " << scalar << std::endl;
  */
  
}


using namespace std;


#ifdef __cplusplus
extern "C" {
#endif 

void testffi( void )
{
  cout << "test c++ ffi" << endl;
  yamlparsetest();
}

#ifdef __cplusplus
}
#endif
