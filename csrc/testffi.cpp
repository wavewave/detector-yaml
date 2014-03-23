#include <iostream>
#include <fstream>
#include <boost/optional/optional.hpp>
#include <boost/variant.hpp>
#include <type_traits>
#include <yaml-cpp/yaml.h>
// 
#include <detector/type.h>


using namespace std;

/********************************/
electron_eff_data_wrapper 
create_electron_eff_data_wrapper( electron_eff_data_t e ) 
{
  electron_eff_data_wrapper ewrap(e);
  return ewrap ;
}

muon_eff_data_wrapper 
create_muon_eff_data_wrapper( muon_eff_data_t e ) 
{
  muon_eff_data_wrapper mwrap(e);
  return mwrap ;
}



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

template<typename T> 
boost::optional< either<import,T> >
importOrDeal( boost::optional<T> getT (YAML::Node node), YAML::Node node ) 
{
  either<import, T> r;   
  boost::optional<string> e1; 
  boost::optional<T> t; 
  if( e1 = maybeFind<string>("Import", node) )
  {
    import imp; 
    imp.name = e1.get();
    r.put(imp);
    return boost::optional< either<import,T> >(r);
  }
  else if( boost::optional<T> t = getT( node))
  {
    r.put(t.get());
    return boost::optional< either<import,T> >(r); 
  } 
  return NULL;
}

boost::optional<electron_eff_data_t> getElectronEffData( YAML::Node node ) 
{ 
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo; 

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo(node) ) )
  { 
    electron_eff_data_t eeff; 
    eeff.name = s1.get(); 
    eeff.meta_info = minfo.get();
    return boost::optional<electron_eff_data_t>(eeff); 
  }
  return NULL;
}

boost::optional<photon_eff_data_t> getPhotonEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) )
  { 
    photon_eff_data_t mueff; 
    mueff.name = s1.get(); 
    mueff.meta_info = minfo.get();
    return boost::optional<photon_eff_data_t>(mueff); 
  }
  return NULL;
}

boost::optional<bjet_eff_data_t> getBJetEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) )
  { 
    bjet_eff_data_t mueff; 
    mueff.name = s1.get(); 
    mueff.meta_info = minfo.get();
    return boost::optional<bjet_eff_data_t>(mueff); 
  }
  return NULL;
}

boost::optional<muon_eff_data_t> getMuonEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) )
  { 
    muon_eff_data_t mueff; 
    mueff.name = s1.get(); 
    mueff.meta_info = minfo.get();
    return boost::optional<muon_eff_data_t>(mueff); 
  }
  return NULL;
}

boost::optional<jet_eff_data_t> getJetEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) )
  { 
    jet_eff_data_t mueff; 
    mueff.name = s1.get(); 
    mueff.meta_info = minfo.get();
    return boost::optional<jet_eff_data_t>(mueff); 
  }
  return NULL;
}

boost::optional<tau_eff_data_t> getTauEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) )
  { 
    tau_eff_data_t mueff; 
    mueff.name = s1.get(); 
    mueff.meta_info = minfo.get();
    return boost::optional<tau_eff_data_t>(mueff); 
  }
  return NULL;
}

boost::optional<object_description_t> getObjectDescription( YAML::Node node)
{
  object_description_t od; 

  boost::optional<YAML::Node> n1, n2, n3, n4, n5, n6, n7 ;
  boost::optional<either <import, electron_eff_data_t > > r1; 
  boost::optional<either <import, photon_eff_data_t > > r2; 
  boost::optional<either <import, bjet_eff_data_t > > r3; 
  boost::optional<either <import, muon_eff_data_t > > r4; 
  boost::optional<either <import, jet_eff_data_t > > r5; 
  boost::optional<either <import, tau_eff_data_t > > r6; 
  // boost::optional<either <import, pt_threshold_eff_data_t > > r7; 
    
  if ( (n1 = maybeFind<YAML::Node>("Electron", node) )
       && (r1 = importOrDeal( getElectronEffData, n1.get() ))
       && (n2 = maybeFind<YAML::Node>("Photon", node) )
       && (r2 = importOrDeal( getPhotonEffData, n2.get()) )
       && (n3 = maybeFind<YAML::Node>("BJet", node) )
       && (r3 = importOrDeal( getBJetEffData, n3.get()) )
       && (n4 = maybeFind<YAML::Node>("Muon", node) )
       && (r4 = importOrDeal( getMuonEffData, n4.get()) )
       && (n5 = maybeFind<YAML::Node>("Jet", node) )
       && (r5 = importOrDeal( getJetEffData, n5.get()) )
       && (n6 = maybeFind<YAML::Node>("Tau", node) )
       && (r6 = importOrDeal( getTauEffData, n6.get()) )
     )
  {
    od.electron = r1.get();
    od.photon = r2.get();
    od.bjet = r3.get();
    od.muon = r4.get();
    od.jet = r5.get();
    od.tau = r6.get();
    return boost::optional<object_description_t>(od);
  }
  
  return NULL;
} 

boost::optional<detector_description_t> getDetectorDescription( YAML::Node doc ) 
{
  detector_description_t dd; 

  boost::optional<string> s1,s2,s3,s4,s5; 
  boost::optional<YAML::Node> s6;
  boost::optional<object_description_t> mo1;

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
    return boost::optional<detector_description_t>(dd);
  }
  return NULL;
}

template <typename T> 
void show_name_and_meta_info( T t ) 
{ 
  static_assert(std::is_base_of<INameable,T>::value, "T must be a descendent of INameable");
  static_assert(std::is_base_of<IMetaInfoable,T>::value, "T must be a descendent of IMetaInfoable"); 
  cout << "-----------------------------" << endl;
  cout << "name is " << t.name() << endl;
  cout << "tag is " << t.meta_info().tag << endl;
  cout << "description is " << t.meta_info().description << endl;
  cout << "comment is " << t.meta_info().comment << endl; 
  cout << "reference is " << t.meta_info().reference << endl; 
  cout << "-----------------------------" << endl;
}

template <typename T, typename TWrapper> 
void show_import_or_do( void f(TWrapper), TWrapper wrap( T )
                      , either<import,T> et )
{
  if( boost::optional<import> t = et.getLeft() ) {
    cout << "----------------------------------" << endl; 
    cout << "import is " << t.get().name << endl;
    cout << "----------------------------------" << endl;
  }
  else if( boost::optional<T> t = et.getRight() ) {
    TWrapper twrapped = wrap(t.get());
    f(twrapped);
  }
  else 
    cout << "parse failed" << endl;
}


void yamlparsetest( void ) {
  std::ifstream input("temp/ATLAS2011.yaml");  
  YAML::Node doc = YAML::Load(input);


  boost::optional<detector_description_t> mdd = getDetectorDescription(doc) ; 

  if( !mdd.is_initialized() ) return; 

  detector_description_t pdd = mdd.get(); 
  cout << "Name is " << pdd.name << endl; 
  cout << "Description is "<< pdd.description << endl;
  cout << "Reference is " << pdd.reference << endl;

  show_import_or_do( show_name_and_meta_info, create_electron_eff_data_wrapper
		     , pdd.object.electron ); 
 
  show_import_or_do( show_name_and_meta_info, create_muon_eff_data_wrapper
                   , pdd.object.muon );
  
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
