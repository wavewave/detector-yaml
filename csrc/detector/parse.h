#ifndef __DETECTOR_PARSE__
#define __DETECTOR_PARSE__

#include <iostream>
#include <boost/optional/optional.hpp>
#include <boost/variant.hpp>
#include <type_traits>
#include <yaml-cpp/yaml.h>
//
#include <detector/type.h>

using namespace std;

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

boost::optional<meta_info_t> getMetaInfo( YAML::Node node ) ;

boost::optional<grid_t> get_grid( vector<double> ptbins, vector<double> etbins, YAML::Node node );

boost::optional<pt_eta_data_t> get_pt_eta_data( YAML::Node node );

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

boost::optional<electron_eff_data_t> getElectronEffData( YAML::Node node ) ;

boost::optional<photon_eff_data_t> getPhotonEffData( YAML::Node node ) ;

boost::optional<photon_eff_data_t> getPhotonEffData( YAML::Node node ) ;

boost::optional<bjet_eff_data_t> getBJetEffData( YAML::Node node ) ;

boost::optional<muon_eff_data_t> getMuonEffData( YAML::Node node ) ;

boost::optional<jet_eff_data_t> getJetEffData( YAML::Node node ) ;

boost::optional<tau_eff_data_t> getTauEffData( YAML::Node node ) ;

boost::optional<object_description_t> getObjectDescription( YAML::Node node) ;

boost::optional<detector_description_t> getDetectorDescription( YAML::Node doc ) ;

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

template <typename T> 
void show_name_and_meta_info_efficiency( T t ) 
{ 
  static_assert(std::is_base_of<IEfficiency,T>::value, "T must be a descendent of IEfficiency");
  cout << "-----------------------------" << endl;
  show_name_and_meta_info( t );
  boost::optional<grid_t> mg ; 
  boost::optional<grid_const_t> mgc; 
  boost::optional<grid_full_t> mgf;

  if( mg = t.efficiency().getGrid() ) { 
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
  cout << "-----------------------------" << endl;
}


template <typename T, typename TWrapper> 
void show_import_or_do( void f(TWrapper), either<import,T> et )
{
  if( boost::optional<import> t = et.getLeft() ) {
    cout << "----------------------------------" << endl; 
    cout << "import is " << t.get().name << endl;
    cout << "----------------------------------" << endl;
  }
  else if( boost::optional<T> t = et.getRight() ) {
    TWrapper w = TWrapper::create_wrapped(t.get());
    f(w);
  }
  else 
    cout << "parse failed" << endl;
}

#endif // __DETECTOR_TYPE__





