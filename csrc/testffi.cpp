#include <iostream>
#include <fstream>
#include <boost/optional/optional.hpp>
#include <boost/variant.hpp>
#include <yaml-cpp/yaml.h>
// 
#include <detector/type.h>


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

void yamlparsetest( void ) {
  std::ifstream input("temp/ATLAS2011.yaml");  
  YAML::Node doc = YAML::Load(input);


  boost::optional<detector_description_t> mdd = getDetectorDescription(doc) ; 

  if( !mdd.is_initialized() ) return; 

  detector_description_t pdd = mdd.get(); 
  cout << "Name is " << pdd.name << endl; 
  cout << "Description is "<< pdd.description << endl;
  cout << "Reference is " << pdd.reference << endl;
  either<import,electron_eff_data_t> e = pdd.object.electron; 
  if( boost::optional<import> t1 = e.getLeft() )
  { 
    cout << "Object.Electron.Import is " << t1.get().name << endl;
  }
  else cout << "Object.Electron parse failed" << endl;
    

  either<import,muon_eff_data_t> mu = pdd.object.muon; 
  if( boost::optional<muon_eff_data_t> t2 = mu.getRight() )
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
