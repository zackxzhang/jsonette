/* json parsing in c++ */


#include <iostream>
#include <string>
#include <unordered_map>
#include <variant>


const std::string SPACES = " \n\r\t";
const std::string DECIMALS = "0123456789";
const std::string HEXADECIMALS = DECIMALS + "abcdefABCDEF";
const std::unordered_map<
    std::string,
    std::string
> ESCAPES = {
    {R"(\\)", "\\"},
    {R"(\")", "\""},
    {R"(\/)", "/" },
    {R"(\b)", "\b"},
    {R"(\f)", "\f"},
    {R"(\n)", "\n"},
    {R"(\r)", "\r"},
    {R"(\t)", "\t"},
};
const std::unordered_map<
    std::string,
    std::variant<bool, std::nullptr_t>
> CONSTANTS = {
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
    const long long unsigned int n;

public:

    Tape(std::string data) : data {data}, n {data.size()} {}

    long long unsigned int i {0};

    const std::string_view operator[] (
        long long unsigned i, long long unsigned int j
    ) {
        return std::string_view(this->data).substr(i, j-i);
    }

    const std::variant<std::string_view, std::nullptr_t> look_ahead() {
        if (this->i+1 >= this->n) {
            return nullptr;
        } else {
            return (*this)[this->i+1, this->i+2];
        }
    }

    const std::variant<std::string_view, std::nullptr_t> look_behind() {
        if (this->i-1 < 0) {
            return nullptr;
        } else {
            return (*this)[this->i-1, this->i];
        }
    }

    const std::variant<std::string_view, std::nullptr_t> read() {
        if (this->i >= this->n) {
            return nullptr;
        } else {
            return (*this)[this->i, this->i+1];
        }
    }

    void step(long long unsigned int k=1) {
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


int main() {

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

}
