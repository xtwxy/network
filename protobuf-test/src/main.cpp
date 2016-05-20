#include <iostream>
#include <iomanip>
#include <sstream>
#include <string>
#include "Header.pb.h"

using namespace std;

int main(int argc, char* argv[])
{
	  // Verify that the version of the library that we linked against is
	  // compatible with the version of the headers we compiled against.
	  GOOGLE_PROTOBUF_VERIFY_VERSION;

	  if (argc != 2) {
	    cerr << "Usage:  " << argv[0] << "<file>" << endl;
	    return -1;
	  }

      std::stringstream output;
	  {
    	  Header header;
    	  header.set_file_code(0xcafebabe);
    	  header.set_file_length(0x10);
    	  header.set_version(0xcadecade);
    	  header.set_shape_type(0xdeadfeed);
	    // Read the existing address book.
	    if (!header.SerializeToOstream(&output)) {
	      cerr << "Failed to serialize to file." << endl;
	      return -1;
	    }
	    cout << "serialized values: " << endl;
	    cout << hex << setw(8) << setfill('0');
	    cout << header.file_code() << endl;
	    cout << header.file_length() << endl;
	    cout << header.version() << endl;
	    cout << header.shape_type() << endl;
	  }

	  {
	    // Read the existing address book.
		  Header header;
	    if (!header.ParseFromIstream(&output)) {
	      cerr << "Failed to parse from file." << endl;
	      return -1;
	    }
	    cout << "parsed values: " << endl;
	    cout << hex << setw(8) << setfill('0');
	    cout << header.file_code() << endl;
	    cout << header.file_length() << endl;
	    cout << header.version() << endl;
	    cout << header.shape_type() << endl;
	  }


	  // Optional:  Delete all global objects allocated by libprotobuf.
	  google::protobuf::ShutdownProtobufLibrary();

  return 0;
}
