#include <iostream>

using namespace std;

int main() {
    string buf;
    bool inInformation = false;
    while(getline(cin, buf)) {
        if (buf == "=== START OF INFORMATION SECTION ===") {
            cout << "information sector\n";
            inInformation = true;
        } else if (buf == "=== START OF INFORMATION SECTION ===") {
            inInformation = false;
        }

        if (inInformation) {
            // Parse input
        }
    }
}
