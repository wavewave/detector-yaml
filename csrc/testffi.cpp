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

template<typename T>
boost::optional<T> maybeFind( string key, YAML::Node doc ) {
  if( doc[key] ) {
    try { 
      T r = doc[key].as<T>(); 
      return boost::optional<T>(r);
    } catch(const YAML::Exception& e) {
      return boost::optional<T>();
    };
  } 
  else return boost::optional<T>();
}

struct detector_description 
{ 
  string name; 
  string description; 
  string reference; 
  string comment;
  string validation_info;
}; 

boost::optional<detector_description> getDetectorDescription( YAML::Node doc ) 
{
  detector_description dd; 

  boost::optional<string> s1 = maybeFind<string>("Name", doc) ; 
  if ( !s1.is_initialized() ) return boost::optional<detector_description>();

  boost::optional<string> s2 = maybeFind<string>("Description", doc) ; 
  if ( !s2.is_initialized() ) return boost::optional<detector_description>();

  boost::optional<string> s3 = maybeFind<string>("Reference", doc) ; 
  if ( !s3.is_initialized() ) return boost::optional<detector_description>();

  boost::optional<string> s4 = maybeFind<string>("Comment", doc) ; 
  if ( !s4.is_initialized() ) return boost::optional<detector_description>();

  boost::optional<string> s5 = maybeFind<string>("ValidationInfo", doc) ; 
  if ( !s5.is_initialized() ) return boost::optional<detector_description>();

  dd.name = *(s1.get_ptr());
  dd.description = *(s2.get_ptr());
  dd.reference = *(s3.get_ptr()); 
  dd.comment = *(s4.get_ptr());
  dd.validation_info = *(s5.get_ptr());


  return boost::optional<detector_description>(dd);

}

void yamlparsetest( void ) {
  std::ifstream input("temp/ATLAS2011.yaml");  
  YAML::Node doc = YAML::Load(input);


  boost::optional<detector_description> mdd = getDetectorDescription(doc) ; 

  if( !mdd.is_initialized() ) return; 

  detector_description const *pdd = mdd.get_ptr(); 
  cout << "Name is " << pdd->name << endl; 
  cout << "Description is "<< pdd->description << endl;
  cout << "Reference is " << pdd -> reference << endl;


  
}


#ifdef __cplusplus
extern "C" {
#endif 

void testffi( void )
{
  yamlparsetest();
}

#ifdef __cplusplus
}
#endif
