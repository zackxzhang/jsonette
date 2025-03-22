/* json parsing in c++ */


#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <variant>


// std::variant<
//     std::nullptr_t,
//     bool,
//     double,
//     std::string, std::vector<JValue>,
//     std::unordered_map<std::string, JValue>
// > JValue;


struct JSON;
using  JArray  = std::vector<JSON>;
using  JObject = std::unordered_map<std::string, JSON>;
using  JValue  = std::variant<
    std::nullptr_t, bool, double, std::string, JArray, JObject
>;
struct JSON {
    JValue value;
};


// void print(const JSON& json, int indent = 0) {
//     std::visit(
//         [indent](const auto& arg) {
//             using T = std::decay_t<decltype(arg)>;
//             if constexpr (std::is_same_v<T, std::nullptr_t>) {
//                 std::cout << "null";
//             } else if constexpr (std::is_same_v<T, bool>) {
//                 std::cout << (arg ? "true" : "false");
//             } else if constexpr (std::is_same_v<T, double>) {
//                 std::cout << arg;
//             } else if constexpr (std::is_same_v<T, std::string>) {
//                 std::cout << "\"" << arg << "\"";
//             } else if constexpr (std::is_same_v<T, JArray>) {
//                 std::cout << "[\n";
//                 for (const auto& elem : arg) {
//                     std::cout << std::string(indent + 2, ' ');
//                     print(elem, indent + 2);
//                     std::cout << ",\n";
//                 }
//                 std::cout << std::string(indent, ' ') << "]";
//             } else if constexpr (std::is_same_v<T, JObject>) {
//                 std::cout << "{\n";
//                 for (const auto& [key, val] : arg) {
//                     std::cout << std::string(indent + 2, ' ')
//                               << "\"" << key << "\": ";
//                     print(val, indent + 2);
//                     std::cout << ",\n";
//                 }
//                 std::cout << std::string(indent, ' ') << "}";
//             }
//         },
//         json.value
//     );
// }


void _print(const JSON& json, int indent);

struct JPrinter {

    int indent;

    void operator()(std::nullptr_t) const {
        std::cout << "null";
    }

    void operator()(bool b) const {
        std::cout << (b ? "true" : "false");
    }

    void operator()(double d) const {
        std::cout << d;
    }

    void operator()(const std::string& s) const {
        std::cout << "\"" << s << "\"";
    }

    void operator()(const JArray& arr) const {
        std::cout << "[\n";
        for (const auto& elem : arr) {
            std::cout << std::string(indent + 2, ' ');
            _print(elem, indent + 2);
            std::cout << ",\n";
        }
        std::cout << std::string(indent, ' ') << "]";
    }

    void operator()(const JObject& obj) const {
        std::cout << "{\n";
        for (const auto& [key, val] : obj) {
            std::cout << std::string(indent + 2, ' ') << "\"" << key << "\": ";
            _print(val, indent + 2);
            std::cout << ",\n";
        }
        std::cout << std::string(indent, ' ') << "}";
    }
};

void _print(const JSON& json, int indent) {
    std::visit(JPrinter{indent}, json.value);
}

void print(const JSON& json, int indent = 0) {
    _print(json, indent);
    std::cout << std::endl;
}


using ull = unsigned long long int;
using nullable_bool = std::variant<bool, std::nullptr_t>;
using nullable_string_view = std::variant<std::string_view, std::nullptr_t>;

constexpr std::string SPACES = " \n\r\t";
constexpr std::string DECIMALS = "0123456789";
constexpr std::string HEXADECIMALS = DECIMALS + "abcdefABCDEF";
const std::unordered_map<std::string, std::string> ESCAPES = {
    {R"(\\)", "\\"},
    {R"(\")", "\""},
    {R"(\/)", "/" },
    {R"(\b)", "\b"},
    {R"(\f)", "\f"},
    {R"(\n)", "\n"},
    {R"(\r)", "\r"},
    {R"(\t)", "\t"},
};
const std::unordered_map<std::string, nullable_bool> CONSTANTS = {
    {"null" , nullptr},
    {"true" , true   },
    {"false", false  },
};


bool includes(const std::string& str, const std::string_view& sub) {
    return str.find(sub) != std::string::npos;
}


class Tape {

private:

    const std::string data;
    const ull n;

public:

    Tape(std::string data) : data {data}, n {data.size()} {}

    ull i {0};

    std::string_view operator[] (ull i, ull j) {
        return std::string_view(this->data).substr(i, j-i);
    }

    nullable_string_view look_ahead() {
        if (this->i+1 >= this->n) {
            return nullptr;
        } else {
            return (*this)[this->i+1, this->i+2];
        }
    }

    nullable_string_view look_behind() {
        if (this->i-1 < 0) {
            return nullptr;
        } else {
            return (*this)[this->i-1, this->i];
        }
    }

    nullable_string_view read() {
        if (this->i >= this->n) {
            return nullptr;
        } else {
            return (*this)[this->i, this->i+1];
        }
    }

    void step(ull k=1) {
        this->i += k;
    }

    void skip() {
        while (this->i < this->n) {
            auto sv = this->read();
            if (
                auto ptr = std::get_if<std::string_view>(&sv);
                ptr && includes(SPACES, *ptr)
            ) {
                this->i += 1;
            } else {
                break;
            }
        }
    }

    bool rear() {
        return this->i >= this->n;
    }

};


class Parser {

private:

    std::string_view number_heads {std::string_view("-0123456789")};
    std::string_view const_heads  {std::string_view("ntf")};

public:

    // nullable_bool read_constant() {

    // }

    // float read_number() {

    // }

    // std::string read_string() {

    // }

    // JArray read_array() {

    // }

    // JObject read_object() {

    // }

    // JValue read_value() {

    // }

    // JSON read() {

    // }

};



int main() {

    auto jstring = JSON {std::string("value1")};
    auto jnumber = JSON {42.0};
    auto jbool   = JSON {true};
    auto jnull   = JSON {nullptr};

    JArray arr;
    arr.push_back(std::move(jbool));
    arr.push_back(std::move(jnull));
    auto jarray = JSON {std::move(arr)};

    JObject obj;
    obj["1st"] = std::move(jstring);
    obj["2nd"] = std::move(jnumber);
    obj["3rd"] = std::move(jarray);
    auto jobject = JSON {std::move(obj)};

    print(jstring);
    print(jnumber);
    print(jbool);
    print(jnull);
    print(jarray);
    print(jobject);

    Tape tape = Tape(R"(  "backslash is \\")");
    std::cout << tape.i << std::endl;
    std::cout << tape[1, 5] << std::endl;
    tape.skip();
    std::cout << tape.i << std::endl;
    auto sv = tape.read();
    auto ptr = std::get_if<std::string_view>(&sv);
    if (ptr) {
        std::cout << *ptr << std::endl;
    }

    std::cout << R"(\\)" << std::endl;
    std::cout <<  "\\"   << std::endl;

    return 0;
}
