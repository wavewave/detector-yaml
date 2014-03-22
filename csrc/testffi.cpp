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

struct import
{
  string name;
};

struct meta_info_t
{
  string tag;
  string description;
  string comment; 
  string reference; 
};

struct electron_eff_data 
{ 
  string name;
  meta_info_t meta_info; 
}; 

struct muon_eff_data
{
  string name;
  meta_info_t meta_info;
};

struct object_description
{ 
  either <import,electron_eff_data> electron;
  either <import,muon_eff_data> muon;
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

boost::optional<meta_info_t> 
getMetaInfo( YAML::Node node ) 
{
  boost::optional<string> s1,s2,s3,s4; 
  if( ( s1 = maybeFind<string>("Tag", node) )
      && ( s2 = maybeFind<string>("Description", node) )
      && ( s3 = maybeFind<string>("Comment", node) )
      && ( s4 = maybeFind<string>("Reference", node) ) )
  {
    meta_info_t meta_info; 
    meta_info.tag = s1.get();
    meta_info.description = s2.get();
    meta_info.comment = s3.get();
    meta_info.reference = s4.get();
    return boost::optional<meta_info_t>(meta_info); 
  }
  return NULL; 
}

boost::optional< either<import,electron_eff_data> >
getElectronEffData( YAML::Node node ) 
{
  either<import, electron_eff_data> r; 
  
  boost::optional<string> e1; 
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo; 
  if( e1 = maybeFind<string>("Import", node) )
  {
    import imp; 
    imp.name = e1.get();
    r.put(imp);
    return r;
  }
  else if( (s1 = maybeFind<string>("Name", node) )
           && ( minfo = getMetaInfo(node) ) )
  { 
    electron_eff_data eeff; 
    eeff.name = s1.get(); 
    eeff.meta_info = minfo.get();
    r.put(eeff);
    return r; 
  }
  return NULL;
}

boost::optional< either<import,muon_eff_data> >
getMuonEffData( YAML::Node node ) 
{
  either<import, muon_eff_data> r; 
  boost::optional<string> e1; 
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  
  if( e1 = maybeFind<string>("Import", node) )
  {
    import imp; 
    imp.name = e1.get();
    r.put(imp);
    return r;
  }
  else if( (s1 = maybeFind<string>("Name", node) )
           && ( minfo = getMetaInfo( node ) ) )
  { 
    muon_eff_data mueff; 
    mueff.name = s1.get(); 
    mueff.meta_info = minfo.get();
    r.put(mueff);
    return r; 
  }
  return NULL;
}


boost::optional<object_description> getObjectDescription( YAML::Node node)
{
  object_description od; 

  boost::optional<YAML::Node> n1;
  boost::optional<YAML::Node> n2;
  boost::optional<either <import, electron_eff_data > > r1; 
  boost::optional<either <import, muon_eff_data > > r2; 

    
  if ( (n1 = maybeFind<YAML::Node>("Electron", node) )
       && (n2 = maybeFind<YAML::Node>("Muon", node) )
       && (r1 = getElectronEffData(n1.get())) 
       && (r2 = getMuonEffData(n2.get()))
     )
  {
    od.electron = r1.get();
    od.muon = r2.get();
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
  std::ifstream input("temp/ATLAS2011.yaml");  
  YAML::Node doc = YAML::Load(input);


  boost::optional<detector_description> mdd = getDetectorDescription(doc) ; 

  if( !mdd.is_initialized() ) return; 

  detector_description const *pdd = mdd.get_ptr(); 
  cout << "Name is " << pdd->name << endl; 
  cout << "Description is "<< pdd->description << endl;
  cout << "Reference is " << pdd -> reference << endl;
  either<import,electron_eff_data> e = pdd->object.electron; 
  if( boost::optional<import> t1 = e.getLeft() )
  { 
    cout << "Object.Electron.Import is " << t1.get().name << endl;
  }
  else cout << "Object.Electron parse failed" << endl;
    

  either<import,muon_eff_data> mu = pdd->object.muon; 
  if( boost::optional<muon_eff_data> t2 = mu.getRight() )
  { 
    cout << "Object.Muon.Eff_data is " << t2.get().name << endl;
    cout << "Object.Muon.EffData.Description " << t2.get().meta_info.description << endl;
  }
  else cout << "Object.Muon parse failed" << endl;

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
