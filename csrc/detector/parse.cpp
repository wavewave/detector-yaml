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

boost::optional<grid_t> 
get_grid( vector<double> ptbins, vector<double> etabins, YAML::Node node )
{
  boost::optional<string> s1; 
  if( s1 = maybeFind<string>("Type",node) ) {
    if( s1.get() == "Const" ) {
      boost::optional<double> mv;
      if( mv = maybeFind<double>("Data",node) ) {
        grid_t g ; 
        grid_const_t gc; 
        gc.pt_eta_bins.pt = ptbins; 
        gc.pt_eta_bins.eta = etabins;
        gc.value = mv.get();
        g.put( gc ); 
        return boost::optional<grid_t>(g);
      }
    } else if( s1.get() == "Full" ) {
      boost::optional<YAML::Node> mvv; 
      if( mvv = maybeFind< YAML::Node > ("Data", node) ) {
	YAML::Node node2 = mvv.get(); // wrong
        if( node2.IsSequence() ) { 
          grid_t g ; 
          grid_full_t gf; 
          gf.pt_eta_bins.pt = ptbins; 
          gf.pt_eta_bins.eta = etabins;
          vector< vector<double> > vv; 
          for(YAML::const_iterator it=node2.begin(); it != node2.end(); ++it ) {
	    vector<double> v; 
            v = it->as< vector<double> >(); 
            vv.push_back(v); 
          }
          gf.grid_data = vv;
          g.put( gf ); 
          return boost::optional<grid_t>(g);
        }
      } 
         
    }
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
      boost::optional<YAML::Node> mnode2; 
      boost::optional< grid_t > mg; 
      if( (ptbin = maybeFind< vector<double> >("PtBins" ,node))
	  && ( etabin = maybeFind< vector<double> >("EtaBins", node)) 
          && ( mnode2 = maybeFind<YAML::Node>("Grid", node) )
          && ( mg = get_grid( ptbin.get(), etabin.get(), mnode2.get() ) ) ) { 
        grid_t g = mg.get() ;
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

boost::optional<tau_eff_detail_t> 
get_tau_eff_detail( YAML::Node node ) 
{
  boost::optional<string> s1; 
  if( s1 = maybeFind<string>("Type",node) ) {
    if( s1.get() == "Tau1or3Prong" ) {
      boost::optional<YAML::Node> mn1,mn2,mn3,mn4;
      boost::optional<pt_eta_data_t> eff1, rej1, eff3, rej3;
      if( ( mn1 = maybeFind<YAML::Node>("Efficiency1Prong", node) )
          && (eff1 = get_pt_eta_data( mn1.get() ) )
          && (mn2 = maybeFind<YAML::Node>("Rejection1Prong", node) )
          && (rej1 = get_pt_eta_data( mn2.get() ) )
          && (mn3 = maybeFind<YAML::Node>("Efficiency3Prong", node) )
          && (eff3 = get_pt_eta_data( mn1.get() ) )
          && (mn4 = maybeFind<YAML::Node>("Rejection3Prong", node) ) 
          && (rej3 = get_pt_eta_data( mn1.get() ) ) ) {
        tau_1or3prong_t t13 ;
        tau_eff_detail_t t; 
        t13.efficiency_1prong = eff1.get();
        t13.rejection_1prong  = rej1.get();
        t13.efficiency_3prong = eff3.get();
        t13.rejection_3prong  = rej3.get();
        t.put( t13 ); 
        return boost::optional<tau_eff_detail_t>(t);
      }
     
    } else if( s1.get() == "TauCombined" ) {
      boost::optional<YAML::Node> mn1,mn2;
      boost::optional<pt_eta_data_t> eff, rej;
      if( ( mn1 = maybeFind<YAML::Node>("Efficiency", node) )
          && (eff = get_pt_eta_data( mn1.get() ) )
          && (mn2 = maybeFind<YAML::Node>("Rejection", node) )
          && (rej = get_pt_eta_data( mn2.get() ) ) ) {
        tau_combined_t tc ;
        tau_eff_detail_t t; 
        tc.efficiency = eff.get();
        tc.rejection  = rej.get();
        t.put( tc ); 
        return boost::optional<tau_eff_detail_t>(t);  
      }
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

boost::optional<track_eff_data_t> getTrackEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<meta_info_t> minfo ;  
  boost::optional<YAML::Node> node2;
  boost::optional<pt_eta_data_t> eff;

  if( (s1 = maybeFind<string>("Name", node) )
      && ( minfo = getMetaInfo( node ) ) 
      && ( node2 = maybeFind<YAML::Node>("Efficiency",node) )
      && ( eff = get_pt_eta_data( node2.get() ) ) ) { 
    track_eff_data_t tr; 
    tr.name = s1.get(); 
    tr.meta_info = minfo.get();
    tr.efficiency = eff.get();
    return boost::optional<track_eff_data_t>(tr); 
  }
  return NULL;
}

boost::optional<tau_eff_data_t> getTauEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<string> s2;
  boost::optional<meta_info_t> minfo ;  
  boost::optional<YAML::Node> n1;
  boost::optional<tau_eff_detail_t> eff; 

  if( (s1 = maybeFind<string>("Name", node) )
      && (s2 = maybeFind<string>("TaggingMethod", node) )
      && ( minfo = getMetaInfo( node ) ) 
      && ( n1 = maybeFind<YAML::Node>("Efficiency", node) )
      && ( eff = get_tau_eff_detail( n1.get() ) ) )
  { 
    tau_eff_data_t t; 
    t.name = s1.get(); 
    t.meta_info = minfo.get();
    t.tag_method = s2.get();
    t.efficiency = eff.get() ;
    return boost::optional<tau_eff_data_t>(t); 
  }
  return NULL;
}

boost::optional<pt_threshold_eff_data_t> getPTThresholdEffData( YAML::Node node ) 
{
  boost::optional<string> s1; 
  boost::optional<double> d1, d2, d3, d4, d5, d6, d7 ;  

  if( (s1 = maybeFind<string>("Name", node) )
      && (d1 = maybeFind<double>("MuPTMIN", node) )
      && (d2 = maybeFind<double>("ElePTMIN", node) )
      && (d3 = maybeFind<double>("PhoPTMIN", node) )
      && (d4 = maybeFind<double>("JetPTMIN", node) )
      && (d5 = maybeFind<double>("BJetPTMIN", node) )
      && (d6 = maybeFind<double>("TrkPTMIN", node) )
      && (d7 = maybeFind<double>("TauPTMIN", node) )  ) { 
    pt_threshold_eff_data_t p; 
    p.name = s1.get(); 
    p.mu_pt_min       = d1.get();
    p.electron_pt_min = d2.get();
    p.photon_pt_min   = d3.get();
    p.jet_pt_min      = d4.get();
    p.bjet_pt_min     = d5.get();
    p.track_pt_min    = d6.get();
    p.tau_pt_min      = d7.get(); 
    return boost::optional<pt_threshold_eff_data_t>(p); 
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
  boost::optional<either <import, pt_threshold_eff_data_t > > r7; 
    
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
       && (n7 = maybeFind<YAML::Node>("PTThresholds", node) )
       && (r7 = importOrDeal( getPTThresholdEffData, n7.get()) )
     )
  {
    od.electron = r1.get();
    od.photon = r2.get();
    od.bjet = r3.get();
    od.muon = r4.get();
    od.jet = r5.get();
    od.tau = r6.get();
    od.ptthresholds = r7.get();
    boost::optional<YAML::Node> mn8;
    if( mn8 = maybeFind<YAML::Node>("Track",node) ) {
      boost::optional< either<import, track_eff_data_t> > eimptrk;  
      eimptrk = importOrDeal( getTrackEffData, mn8.get() ); 
      od.track = eimptrk;
    } else { 
      boost::optional< either<import, track_eff_data_t> > nullimptrk; 
      od.track = nullimptrk; 
    } 

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


void show_pt_eta_data( pt_eta_data_t dat ) 
{
  boost::optional<grid_t> mg ; 
  boost::optional<grid_const_t> mgc; 
  boost::optional<grid_full_t> mgf;

  if( mg = dat.getGrid() ) { 
    if( mgc = mg.get().getGridConst() ) {
      grid_const_t gc = mgc.get();
    
      cout << "efficiency : pt_bins = [" ;
      for( auto it = gc.pt_eta_bins.pt.begin() ; it != gc.pt_eta_bins.pt.end() ; ++it ) {
        cout << *it << "," ;
      }
      cout << "] " << endl;
      cout << "efficiency : eta_bins = [";
      for( auto it = gc.pt_eta_bins.eta.begin() ; it != gc.pt_eta_bins.eta.end() ; ++it ) {
        cout << *it << "," ;
      }
      cout << "] " << endl;
      cout << "efficiency : value = " << gc.value << endl; 
    } 
    else if( mgf = mg.get().getGridFull() ) {
      grid_full_t gf = mgf.get();
    
      cout << "efficiency : pt_bins = [" ;
      for( auto it = gf.pt_eta_bins.pt.begin() ; it != gf.pt_eta_bins.pt.end() ; ++it ) {
        cout << *it << "," ;
      }
      cout << "] " << endl;
      cout << "efficiency : eta_bins = [";
      for( auto it = gf.pt_eta_bins.eta.begin() ; it != gf.pt_eta_bins.eta.end() ; ++it ) {
        cout << *it << "," ;
      }
      cout << "] " << endl;
      cout << "efficiency : grid_data = [ " ;
      for( auto it1 = gf.grid_data.begin() ; it1 != gf.grid_data.end() ; ++it1 ) {
        cout << "[" ; 
        for( auto it2 = it1->begin() ; it2 != it1->end() ; ++it2 ) {
          cout << *it2 << "," ; 
        }
        cout << "],  " << endl << "                           " ;
      } 
      cout << "] " << endl;

    }
  } 
}


void ptshow( PTThresholdWrapper p ) {
  cout << "name : "            <<  p.dat.name << endl; 
  cout << "mu_pt_min : "       << p.dat.mu_pt_min << endl;
  cout << "electron_pt_min : " << p.dat.electron_pt_min << endl; 
  cout << "photon_pt_min : "   << p.dat.photon_pt_min << endl; 
  cout << "jet_pt_min : "      << p.dat.jet_pt_min << endl;
  cout << "bjet_pt_min : "     << p.dat.bjet_pt_min << endl;
  cout << "track_pt_min : "    << p.dat.track_pt_min << endl; 
  cout << "tau_pt_min : "      << p.dat.tau_pt_min << endl;
}


void taushow( TauWrapper p ) {
  show_name_and_meta_info( p );
  cout << "tag_method : " << p.dat.tag_method << endl;
  tau_eff_detail_t eff = p.dat.efficiency; 
  boost::optional<tau_1or3prong_t> mt ;
  boost::optional<tau_combined_t> mtc;
  if( mt = eff.get1or3Prong() ) {
    tau_1or3prong_t t = mt.get();
    show_pt_eta_data( t.efficiency_1prong ); 
    show_pt_eta_data( t.rejection_1prong ); 
    show_pt_eta_data( t.efficiency_3prong ); 
    show_pt_eta_data( t.rejection_3prong ); 
  } else if ( mtc = eff.getCombined() ) {
    tau_combined_t t = mtc.get();
    show_pt_eta_data( t.efficiency );
    show_pt_eta_data( t.rejection );
  }
  cout << "----------------------------------" << endl;
}
