#include <iostream>
#include <fstream>
#include <boost/optional/optional.hpp>
#include "yaml-cpp/yaml.h"

using namespace std;


/********************************/

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

template<>
boost::optional<YAML::Node> maybeFind( string key, YAML::Node doc ) {
  if( doc[key] ) {
    return boost::optional<YAML::Node>(doc[key]);
  }
  else return boost::optional<YAML::Node>();
}

struct object_description
{ 
  string name;
};

struct detector_description 
{ 
  string name; 
  string description; 
  string reference; 
  string comment;
  string validation_info;
  object_description object;
}; 

boost::optional<object_description> getObjectDescription( YAML::Node node)
{
  object_description od; 

  boost::optional<string> s1 = maybeFind<string>("Name", node) ; 
  if ( !s1.is_initialized() ) return boost::optional<object_description>();

  od.name = *(s1.get_ptr());

  return boost::optional<object_description>(od);
} 



boost::optional<detector_description> getDetectorDescription( YAML::Node doc ) 
{
  detector_description dd; 
  
  //if ( !s1.is_initialized() ) return boost::optional<detector_description>();

  if( boost::optional<string> s1 = maybeFind<string>("Name", doc) ) {  
    if( boost::optional<string> s2 = maybeFind<string>("Description", doc) ) { 
      if( boost::optional<string> s3 = maybeFind<string>("Reference", doc) ) {
        if( boost::optional<string> s4 = maybeFind<string>("Comment", doc) ) {
          if( boost::optional<string> s5 = maybeFind<string>("ValidationInfo", doc) ) {
            if( boost::optional<YAML::Node> s6 = maybeFind<YAML::Node>("Object", doc) ) {
              dd.name = s1.get();
              dd.description =s2.get();
	      dd.reference = s3.get(); 
	      dd.comment = s4.get();
	      dd.validation_info = *(s5.get_ptr());

	      YAML::Node n1 = *(s6.get_ptr());
	      boost::optional<object_description> mo1 = getObjectDescription(n1); 
	      if( !mo1.is_initialized() ) return boost::optional<detector_description>();

	      dd.object = *(mo1.get_ptr());

	      return boost::optional<detector_description>(dd);
	    }
          }
        }
      }
    }
  }
  
  return NULL;
}




void yamlparsetest( void ) {
  std::ifstream input("ATLAS2011.yaml");  
  YAML::Node doc = YAML::Load(input);


  boost::optional<detector_description> mdd = getDetectorDescription(doc) ; 

  if( !mdd.is_initialized() ) return; 

  detector_description const *pdd = mdd.get_ptr(); 
  cout << "Name is " << pdd->name << endl; 
  cout << "Description is "<< pdd->description << endl;
  cout << "Reference is " << pdd -> reference << endl;
  cout << "Object.Name is " << pdd->object.name << endl;

  
}


void yamlemittest( void ) {
    YAML::Emitter out;
    out << "Hello, World!";
   
    std::cout << "Here's the output YAML:\n" << out.c_str(); // prints "Hello, World!"
}

/*****************************************/

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
