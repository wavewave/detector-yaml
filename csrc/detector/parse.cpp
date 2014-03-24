#include <iostream>
#include <boost/optional/optional.hpp>
#include <boost/variant.hpp>
#include <type_traits>
#include <yaml-cpp/yaml.h>
// 
#include <detector/type.h>
#include <detector/parse.h>

using namespace std;

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

boost::optional<pt_eta_data_t>
get_pt_eta_data( YAML::Node node )
{
  boost::optional<string> s1; 
  if( s1 = maybeFind<string>("Type",node) ) {
    if( s1.get() == "Grid" ) {
      boost::optional< vector<double> > ptbin; 
      boost::optional< vector<double> > etabin;
      if( (ptbin = maybeFind< vector<double> >("PtBins" ,node))
	  && ( etabin = maybeFind< vector<double> >("EtaBins", node)) ) { 
        grid_t g ;
        g.pt_bins = ptbin.get();
        g.eta_bins = etabin.get(); 
        pt_eta_data_t dat; 
        dat.put(g);
        return boost::optional<pt_eta_data_t>(dat);
      }
    }
    else if ( s1.get() == "Interpolation" ) {
    }
  
  }
  
  return NULL; 
}


boost::optional<electron_eff_data_t> getElectronEffData( YAML::Node node ) 
{ 
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo; 
  boost::optional<YAML::Node> node2;
  boost::optional<pt_eta_data_t> eff;

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo(node) )
      && ( node2 = maybeFind<YAML::Node>("Efficiency",node) )
      && ( eff = get_pt_eta_data( node2.get() ) ) ) { 
    electron_eff_data_t e; 
    e.name = s1.get(); 
    e.meta_info = minfo.get();
    e.efficiency = eff.get();
    return boost::optional<electron_eff_data_t>(e); 
  }
  return NULL;
}

boost::optional<photon_eff_data_t> getPhotonEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  
  boost::optional<YAML::Node> node2;
  boost::optional<pt_eta_data_t> eff;

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) 
      && ( node2 = maybeFind<YAML::Node>("Efficiency",node) )
      && ( eff = get_pt_eta_data( node2.get() ) ) ) { 
    photon_eff_data_t p; 
    p.name = s1.get(); 
    p.meta_info = minfo.get();
    p.efficiency = eff.get();
    return boost::optional<photon_eff_data_t>(p); 
  }
  return NULL;
}

boost::optional<bjet_eff_data_t> getBJetEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  
  boost::optional<YAML::Node> node2;
  boost::optional<pt_eta_data_t> eff;
  boost::optional<YAML::Node> node3;
  boost::optional<pt_eta_data_t> rej;

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) 
      && ( node2 = maybeFind<YAML::Node>("Efficiency",node) )
      && ( eff = get_pt_eta_data( node2.get() ) ) 
      && ( node3 = maybeFind<YAML::Node>("Rejection",node) )
      && ( rej = get_pt_eta_data( node3.get() ) ) ) { 
    bjet_eff_data_t b; 
    b.name = s1.get(); 
    b.meta_info = minfo.get();
    b.efficiency = eff.get();
    b.rejection = rej.get();
    return boost::optional<bjet_eff_data_t>(b); 
  }
  return NULL;
}

boost::optional<muon_eff_data_t> getMuonEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  
  boost::optional<YAML::Node> node2;
  boost::optional<pt_eta_data_t> eff;

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) 
      && ( node2 = maybeFind<YAML::Node>("Efficiency",node) )
      && ( eff = get_pt_eta_data( node2.get() ) ) )
  { 
    muon_eff_data_t m; 
    m.name = s1.get(); 
    m.meta_info = minfo.get();
    m.efficiency = eff.get();
    return boost::optional<muon_eff_data_t>(m); 
  }
  return NULL;
}

boost::optional<jet_eff_data_t> getJetEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  
  boost::optional<YAML::Node> node2;
  boost::optional<pt_eta_data_t> eff;

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) 
      && ( node2 = maybeFind<YAML::Node>("Efficiency",node) )
      && ( eff = get_pt_eta_data( node2.get() ) ) ) { 
    jet_eff_data_t j; 
    j.name = s1.get(); 
    j.meta_info = minfo.get();
    j.efficiency = eff.get();
    return boost::optional<jet_eff_data_t>(j); 
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
