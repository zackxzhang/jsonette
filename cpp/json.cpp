/* json parsing in c++ */


#include <iostream>
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


std::string fromCodePoint(uint32_t cp) {
    std::string str;
    if (cp <= 0x7F) {
        // 1-byte sequence: 0xxxxxxx
        str.push_back(static_cast<char>(cp));
    } else if (cp <= 0x7FF) {
        // 2-byte sequence: 110xxxxx 10xxxxxx
        str.push_back(static_cast<char>(0xC0 | ((cp >> 6) & 0x1F)));
        str.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    } else if (cp <= 0xFFFF) {
        // 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
        str.push_back(static_cast<char>(0xE0 | ((cp >> 12) & 0x0F)));
        str.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        str.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    } else if (cp <= 0x10FFFF) {
        // 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
        str.push_back(static_cast<char>(0xF0 | ((cp >> 18) & 0x07)));
        str.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3F)));
        str.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        str.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    }
    return str;
}


constexpr std::string SPACES   = " \n\r\t";
constexpr std::string DECIMALS = "0123456789";
const std::string HEXADECIMALS = "0123456789abcdefABCDEF";
const std::unordered_map<char, char> ESCAPES = {
    {'\\', '\\'},
    {'"', '"'},
    {'/', '/'},
    {'b', '\b'},
    {'f', '\f'},
    {'n', '\n'},
    {'r', '\r'},
    {'t', '\t'},
};


using nullable_string_view = std::variant<std::string_view, std::nullptr_t>;

bool exists(const nullable_string_view nsv) {
    auto ptr = std::get_if<std::string_view>(&nsv);
    if (ptr) {
        return true;
    } else {
        return false;
    }
}

bool equals(const char ch, const nullable_string_view nsv) {
    if (exists(nsv)) {
        auto sv = std::get<std::string_view>(nsv);
        if (sv.size() == 1 && sv[0] == ch) {
            return true;
        }
    }
    return false;
}

bool includes(const std::string& str, const nullable_string_view nsv) {
    if (exists(nsv)) {
        auto sv = std::get<std::string_view>(nsv);
        return str.find(sv) != std::string::npos;
    }
    return false;
}


class Tape {

private:

    const std::string data;
    const size_t n;

public:

    Tape(std::string data) : data {data}, n {data.size()} {}

    size_t i {0};

    char operator[](size_t i) const {
        return this->data[i];
    }

    std::string_view operator[](size_t i, size_t j) const {
        return std::string_view(this->data).substr(i, j-i);
    }

    nullable_string_view look_ahead() const {
        if (this->i+1 >= this->n) {
            return nullptr;
        } else {
            return (*this)[this->i+1, this->i+2];
        }
    }

    nullable_string_view look_behind() const {
        if (this->i-1 < 0) {
            return nullptr;
        } else {
            return (*this)[this->i-1, this->i];
        }
    }

    nullable_string_view read() const {
        if (this->i >= this->n) {
            return nullptr;
        } else {
            return (*this)[this->i, this->i+1];
        }
    }

    void step(size_t k=1) {
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

    bool rear() const {
        return this->i >= this->n;
    }

};


class Parser {

private:

    std::string number_heads {std::string("-0123456789")};

public:

    std::nullptr_t read_null(Tape& tape) {
        size_t i = tape.i;
        if ("null" == tape[i, i+4]) {
            tape.step(4);
            return nullptr;
        } else {
            throw std::runtime_error("not a null");
        }
    }

    bool read_bool(Tape& tape) {
        size_t i = tape.i;
        if ("true" == tape[i, i+4]) {
            tape.step(4);
            return true;
        } else if ("false" == tape[i, i+5]) {
            tape.step(5);
            return false;
        } else {
            throw std::runtime_error("not a bool");
        }
    }

    double read_number(Tape& tape) {
        size_t start = tape.i;
        bool fraction = false;
        bool exponent = false;
        auto c = tape.read();
        if (includes(std::string("-123456789"), c)) {
            // pass
        } else if (equals('0', c)) {
            if (includes(DECIMALS, tape.look_ahead())) {
                throw std::runtime_error("invalid number");
            }
        } else {
            throw std::runtime_error("not a number");
        }
        tape.step();
        while (true) {
            auto c = tape.read();
            if (includes(DECIMALS, c)) {
                // pass
            } else if (equals('.', c)) {
                if (
                    fraction ||
                    exponent ||
                    !includes(DECIMALS, tape.look_behind())
                ) {
                    throw std::runtime_error("invalid decimal point");
                }
                fraction = true;
            } else if (includes(std::string("eE"), c)) {
                if (exponent || !includes(DECIMALS, tape.look_behind())) {
                    throw std::runtime_error("invalid exponent");
                }
                exponent = true;
            } else if (includes(std::string("+-"), c)) {
                if (!includes(std::string("eE"), tape.look_behind())) {
                    throw std::runtime_error("sign must follow exponent");
                }
            } else {
                break;
            }
            tape.step();
        }
        size_t end = tape.i;
        std::string number {tape[start, end]};
        try {
            if (fraction || exponent) {
                return std::stod(number);
            } else {
                return static_cast<double>(std::stoi(number));
            }
        } catch (const std::exception &) {
            throw std::runtime_error("failed to parse number");
        }
    }

    std::string read_string(Tape& tape) {
        std::string str;
        auto c = tape.read();
        if (!equals('"', c)) {
            throw std::runtime_error("not a string");
        }
        tape.step();
        while (true) {
            auto c = tape.read();
            tape.step();
            if (!exists(c)) {
                throw std::runtime_error("string not complete");
            } else if (equals('"', c)) {
                break;
            } else if (equals('\\', c)) {
                if (includes(R"("\/bfnrt)", tape.read())) {
                    str.push_back(ESCAPES.at(tape[tape.i]));
                    tape.step();
                } else if (equals('u', tape.read())) {
                    tape.step();
                    int u;
                    try {
                        std::string ufff {tape[tape.i, tape.i+4]};
                        u = std::stoi(ufff, nullptr, 16);
                    } catch (const std::exception &) {
                        throw std::runtime_error("bad hexadecimals");
                    }
                    if (u < 0 || u > 0x10FFFF) {
                        throw std::out_of_range("unvalid unicode code point");
                    }
                    str.append(fromCodePoint(static_cast<uint32_t>(u)));
                    tape.step(4);
                } else {
                    throw std::runtime_error("bad escape");
                }
            } else {
                str.append(std::get<std::string_view>(c));
            }
        }
        return str;
    }

    JArray read_array(Tape& tape) {
        JArray arr;
        auto c = tape.read();
        if (!equals('[', c)) {
            throw std::runtime_error("not an array");
        }
        tape.step();
        tape.skip();
        c = tape.read();
        if (!exists(c)) {
            throw std::runtime_error("array not complete");
        }
        else if (equals(']', c)) {
            tape.step();
        }
        else {
            while (true) {
                tape.skip();
                arr.push_back(std::move(this->read(tape)));
                tape.skip();
                auto c = tape.read();
                if (!exists(c)) {
                    throw std::runtime_error("array not complete");
                } else if (equals(',', c)) {
                    tape.step();
                } else if (equals(']', c)) {
                    tape.step();
                    break;
                } else {
                    throw std::runtime_error("array missing , or ]");
                }
            }
        }
        return arr;
    }

    JObject read_object(Tape& tape) {
        JObject obj;
        auto c = tape.read();
        if (!equals('{', c)) {
            throw std::runtime_error("not an object");
        }
        tape.step();
        tape.skip();
        c = tape.read();
        if (!exists(c)) {
            throw std::runtime_error("object not complete");
        } else if (equals('}', c)) {
            tape.step();
        } else {
            while (true) {
                tape.skip();
                auto key = this->read_string(tape);
                tape.skip();
                auto c = tape.read();
                if (!exists(c)) {
                    throw std::runtime_error("object not complete");
                } else if (!equals(':', c)) {
                    throw std::runtime_error("object missing :");
                }
                tape.step();
                tape.skip();
                obj[key] = std::move(this->read(tape));
                tape.skip();
                c = tape.read();
                if (!exists(c)) {
                    throw std::runtime_error("object not complete");
                } else if (equals(',', c)) {
                    tape.step();
                } else if (equals('}', c)) {
                    tape.step();
                    break;
                } else {
                    throw std::runtime_error("object missing , or }");
                }
            }
        }
        return obj;
    }

    JSON read(Tape& tape) {
        auto c = tape.read();
        if (equals('n', c)) {
            return JSON { std::move(this->read_null(tape)) };
        } else if (includes("tf", c)) {
            return JSON { std::move(this->read_bool(tape)) };
        } else if (includes(this->number_heads, c)) {
            return JSON { std::move(this->read_number(tape)) };
        } else if (equals('"', c)) {
            return JSON { std::move(this->read_string(tape)) };
        } else if (equals('[', c)) {
            return JSON { std::move(this->read_array(tape)) };
        } else if (equals('{', c)) {
            return JSON { std::move(this->read_object(tape)) };
        } else {
            throw std::runtime_error("bad value");
        }
    }

    JSON operator()(const std::string& data) {
        auto tape = Tape(data);
        tape.skip();
        auto json = std::move(this->read(tape));
        tape.skip();
        if (!tape.rear()) {
            throw std::runtime_error("bad remainder");
        }
        return json;
    }
};


int main() {

    // Q: why operate on raw strings instead of regular strings?
    // A: so that escape sequences like linefeeds and unicodes are not decoded.
    std::string a {"\u03A9"};
    std::string b {R"(\u03A9)"};
    std::cout << std::format(R"("{}".size() = {})", a, a.size()) << std::endl;
    std::cout << std::format(R"("{}".size() = {})", b, b.size()) << std::endl;

    /* json structure */
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

    /* json parser */
    auto parser = Parser {};

    // unit test
    auto t1 = Tape {R"(null)"};
    auto t2 = Tape {R"(true ,)"};
    auto t3 = Tape {R"(falsed)"};
    std::cout << parser.read_null(t1) << std::endl;
    std::cout << parser.read_bool(t2) << std::endl;
    std::cout << parser.read_bool(t3) << std::endl;

    auto t4 = Tape {R"(0)"};
    auto t5 = Tape {R"(5)"};
    auto t6 = Tape {R"(12.40)"};
    auto t7 = Tape {R"(-3.2e5 )"};
    std::cout << parser.read_number(t4) << std::endl;
    std::cout << parser.read_number(t5) << std::endl;
    std::cout << parser.read_number(t6) << std::endl;
    std::cout << parser.read_number(t7) << std::endl;

    auto t8  = Tape(R"("little")");
    auto t9  = Tape(R"("big",  0.9)");
    auto t10 = Tape(R"("\u03A9")");
    std::cout << parser.read_string(t8)  << std::endl;
    std::cout << parser.read_string(t9)  << std::endl;
    std::cout << parser.read_string(t10) << std::endl;

    auto t11 = Tape(R"("backslash is \\")");
    auto t12 = Tape("\"backslash is \\\\\"");
    std::cout << parser.read_string(t11) << std::endl;
    std::cout << parser.read_string(t12) << std::endl;

    auto t13 = Tape(R"([null,  true,  [ "hello" ,   {"json": 1.2e-3}   ]  ] )");
    auto t14 = Tape(R"({"a":1, "bcd":  { "json": false, "xml":[null, 2e-3]}})");
    print(JSON {parser.read_array(t13)} );
    print(JSON {parser.read_object(t14)});

    // integration test
    std::vector<std::string> examples = {
        R"(  -3.2e5 )",
        R"(["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"])",
        R"(
        {
            "employee": {
                "name":       "\u03A9 Smith",
                "salary":     56000,
                "married":    true
                }
        }
        )",
    };
    for (const auto& example : examples) {
        print(parser(example));
    }

    return 0;
}
