#include <iostream>

using namespace std;

#ifdef __cplusplus
extern "C" {
#endif 

void testffi( void )
{
  cout << "test c++ ffi" << endl;
}

#ifdef __cplusplus
}
#endif
