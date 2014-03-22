#include <iostream>
#include <fstream>
#include <boost/optional/optional.hpp>
#include <boost/variant.hpp>
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

struct import
{
  string name;
};

template<typename A, typename B>
class either 
{
private: 
  enum which_t { left, right } which;
  boost::variant<A,B> content; 
public: 
  void put(A a) { which = left; content = a; } 
  void put(B b) { which = right; content = b; }

  bool isLeft( void ) { return (which == left); }
  bool isRight( void ) { return (which == right); }

  boost::optional<A> getLeft ( void ) 
  {
    if( which == left ) {
      return boost::optional<A>(boost::get<A>(content)); 
    }
    else return boost::optional<A>(); 
  }
  boost::optional<B> getRight( void )
  {
    if( which == right ) 
    {
      return boost::optional<B>(boost::get<B>(content));
    }
    else return boost::optional<B>();
  }
};

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

  boost::optional<string> s1;
  if ( s1 = maybeFind<string>("Name", node) ) 
  {
    od.name = s1.get();

    return boost::optional<object_description>(od);
  }
  
  return NULL;
} 

boost::optional<detector_description> getDetectorDescription( YAML::Node doc ) 
{
  detector_description dd; 

  boost::optional<string> s1,s2,s3,s4,s5; 
  boost::optional<YAML::Node> s6;
  boost::optional<object_description> mo1;

  if( (s1 = maybeFind<string>("Name", doc))
      && (s2 = maybeFind<string>("Description", doc)) 
      && (s3 = maybeFind<string>("Reference", doc))
      && (s4 = maybeFind<string>("Comment", doc))
      && (s5 = maybeFind<string>("ValidationInfo", doc))
      && (s6 = maybeFind<YAML::Node>("Object", doc)) 
      && (mo1 = getObjectDescription(s6.get()))

    ) 
  {
    dd.name = s1.get();
    dd.description =s2.get();
    dd.reference = s3.get(); 
    dd.comment = s4.get();
    dd.validation_info = s5.get();
    dd.object = mo1.get();
    return boost::optional<detector_description>(dd);
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

  either<int,string> t ;
  // t.put( "hello" );
  t.put( 32 );
  boost::optional<int> tl = t.getLeft();   

  if( tl.is_initialized() ) {
    cout << "tl : " << tl.get() << endl; 
  } 
  else {
    cout << "tl is not initialized" << endl;
  }

  //<< ":" << t.isRight() << endl;  
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
