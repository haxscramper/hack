#include <locale>
#include <codecvt>
#include <iomanip>
#include <string>
#include <iostream>

int main() {
    setlocale(LC_ALL, "en_US.utf8");

    std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
    std::string  text = "«HEllo«oåäö马克";
    std::wstring wide = converter.from_bytes(text);
    std::wcout << wide << std::endl;
    for (const auto& ch : wide) {
        std::wcout << std::setw(8) << std::left << (unsigned int)ch << ch
                   << "\n";
    }
    std::cout << std::endl;
    std::wcout << "----------------";
    std::wcout << std::endl;

    for (wchar_t ch : U"«HEllo«oåäö马克") {
        std::wcout << "-: " << std::setw(8) << std::left
                   << (unsigned int)ch << ch << "\n";
    }
    std::cout << std::endl;
}
