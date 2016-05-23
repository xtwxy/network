
#include <algorithm>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#include "echo_service.hpp"

#include <RCF/RCF.hpp>

int main(int argc, char* argv[])
{
    RCF::RcfInitDeinit rcfInit;

    if(argc != 2) {
      std::cout << "Usage: " << argv[0] << " <HOST/IP>" << std::endl;
    }
    try
    {
        std::string s = "Hello, World! \
000000000000c5a8 T _init \
000000000000d7c0 t deregister_tm_clones \
000000000000d7f0 t register_tm_clones \
000000000000d830 t __do_global_dtors_aux \
000000000000d870 t frame_dummy \
000000000000d8a8 T pocoBuildManifest \
000000000000da1a t _Z41__static_initialization_and_destruction_0ii \
000000000000da62 t _GLOBAL__sub_I_SimpleAuth.cpp \
000000000000da77 W _ZNSt11char_traitsIcE7compareEPKcS2_m \
000000000000daa4 W _ZnwmPv \
000000000000dab6 W _ZdlPvS_ \
000000000000dac4 W _ZN4Poco13AtomicCounterppEv \
000000000000dade W _ZN4Poco13AtomicCountermmEv \
000000000000dafe W _ZNK4Poco16RefCountedObject9duplicateEv \
000000000000db1c W _ZNK4Poco16RefCountedObject7releaseEv \
000000000000dbb6 W _ZNKSt9type_info4nameEv \
000000000000dbe5 W _ZNSt18_Rb_tree_node_base10_S_minimumEPS_ \
000000000000dc0e W _ZNSt18_Rb_tree_node_base10_S_maximumEPS_ \
000000000000dc38 W _ZNK4Poco3OSP13BundleContext8registryEv \
000000000000dc4a W _ZNK4Poco15StringTokenizer5beginEv \
000000000000dc64 W _ZNK4Poco15StringTokenizer3endEv \
000000000000dc7e W _ZN4Poco12DigestEngine6updateERKSs \
000000000000dcd6 W _ZNSt3setISsSt4lessISsESaISsEED1Ev \
000000000000dcd6 W _ZNSt3setISsSt4lessISsESaISsEED2Ev \
000000000000dcf0 W _ZN17SimpleAuthServiceC1ERKSsS1_S1_S1_RKSt3setISsSt4lessISsESaISsEES1_ \
000000000000dea0 W _ZN17SimpleAuthServiceD1Ev \
000000000000e042 W _ZTv0_n24_N17SimpleAuthServiceD1Ev \
000000000000e04e W _ZN17SimpleAuthServiceD0Ev \
000000000000e074 W _ZTv0_n24_N17SimpleAuthServiceD0Ev \
000000000000e07e W _ZNK17SimpleAuthService12authenticateERKSsS1_ \
000000000000e1c4 W _ZNK17SimpleAuthService9authorizeERKSsS1_ \
000000000000e298 W _ZNK17SimpleAuthService4typeEv \
000000000000e2aa W _ZNK17SimpleAuthService3isAERKSt9type_info \
000000000000e38a W _ZNK17SimpleAuthService15hashCredentialsERKSs \
000000000000e454 W _ZN25SimpleAuthBundleActivatorC1Ev \
000000000000e454 W _ZN25SimpleAuthBundleActivatorC2Ev \
000000000000e4b2 W _ZN25SimpleAuthBundleActivatorD1Ev \
000000000000e4b2 W _ZN25SimpleAuthBundleActivatorD2Ev \
000000000000e526 W _ZN25SimpleAuthBundleActivatorD0Ev \
000000000000e54c W _ZN25SimpleAuthBundleActivator5startEN4Poco7AutoPtrINS0_3OSP13BundleContextEEE \
000000000000f3b0 W _ZN25SimpleAuthBundleActivator4stopEN4Poco7AutoPtrINS0_3OSP13BundleContextEEE \
000000000000f42a W _ZN4Poco7AutoPtrINS_3OSP10ServiceRefEEC1ERKS3_ \
000000000000f42a W _ZN4Poco7AutoPtrINS_3OSP10ServiceRefEEC2ERKS3_ \
000000000000f466 W _ZN4Poco7AutoPtrINS_3OSP10ServiceRefEED1Ev \
000000000000f466 W _ZN4Poco7AutoPtrINS_3OSP10ServiceRefEED2Ev";

        // Make the call.
        RcfClient<echo_service> service( RCF::TcpEndpoint(argv[1], 50001) );
        while(true) {
		std::string s = service.echo(s);
	}
    }
    catch(const RCF::Exception & e)
    {
        std::cout << "Caught exception:\n";
        std::cout << e.getError().getErrorString() << std::endl;
        return 1;
    }

    return 0;
}
