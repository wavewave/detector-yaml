#include <iostream>
#include <fstream>
#include <boost/optional/optional.hpp>
#include "yaml-cpp/yaml.h"

using namespace std;

void yamlemittest( void ) {
    YAML::Emitter out;
    out << "Hello, World!";
   
    std::cout << "Here's the output YAML:\n" << out.c_str(); // prints "Hello, World!"
}

boost::optional<string> maybeTextFind( string key, YAML::Node doc ) {
  if( doc[key] ) {
    try { 
      string r = doc[key].as<string>(); 
      return boost::optional<string>(r);
    } catch(const YAML::Exception& e) {
      return boost::optional<string>();
    };
  } 
  else return boost::optional<string>();
}

void yamlparsetest( void ) {
  std::ifstream input("temp/ATLAS2011.yaml");  
  YAML::Node doc = YAML::Load(input);

  boost::optional<string> s = maybeTextFind("Name", doc) ; 

  if( s.is_initialized() ) {
    string const* r = s.get_ptr(); 
    cout << "Name is : " << (*r) << endl; 
  }


  // std::cout << doc << "\n";

    


  /*  YAML::Parser parser(fin);

  YAML::Node doc;
  parser.GetNextDocument(doc); 
  std::string scalar; 
  doc >> scalar; 
  std::cout << "That scalar was: " << scalar << std::endl;
  */
  
}


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
