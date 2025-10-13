#include <iostream>
#include <string>
#include <vector>
#include <optional>
#include <unordered_map>
#include <tuple>
#include <utility>
#include <type_traits>
#include <functional>
#include <sstream>
#include <iomanip>
#include <cmath>
#include <algorithm>
#include <typeinfo>
#include <cstring>

using namespace std;

// helpers
template<typename T> static inline std::ostream& operator<<(std::ostream& os, const std::vector<T>& v);
template<typename T> static inline std::string to_str(const T& v){ std::ostringstream os; os<<v; return os.str(); }
static inline std::string to_str(const std::string& s){ return s; }
static inline std::string to_str(const char* s){ return std::string(s); }
static inline std::string to_str(double v){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1)<<v; return os.str(); }
static inline std::string to_str(float v){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1)<<v; return os.str(); }
template<typename T> static inline int to_int(const T& v){ if constexpr (std::is_convertible_v<T,std::string>) return std::stoi(v); else return static_cast<int>(v); }
template<typename T> static inline float to_float(const T& v){ if constexpr (std::is_convertible_v<T,std::string>) return std::stof(v); else return static_cast<float>(v); }
template<typename C> static inline int sum(const C& c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline int sum(std::initializer_list<T> c){ int s=0; for(const auto& e: c) s += e; return s; }
template<typename T> static inline size_t len(const T& c){ return c.size(); }
static inline size_t len(const char* s){ return std::char_traits<char>::length(s); }
template<typename K, typename V> static inline std::string to_str(const std::unordered_map<K,V>&){ return std::string("{...}"); }
// minimal ABC base to support 'class X : public ABC'
struct ABC {};
// minimal math namespace replacement for 'math.pi'
struct __fluxus_math { static constexpr double pi = 3.141592653589793; static inline double sqrt(double x){ return std::sqrt(x); } } math;
// minimal sys module stub
namespace sys { static std::vector<std::string> argv; }
namespace asyncio { template<typename F> static inline void run(F f){ f(); } }

// vector to string conversion for printing
template<typename T>
static inline std::string vec_to_str(const std::vector<T>& v) {
    std::ostringstream os;
    os << "[";
    for (size_t i = 0; i < v.size(); ++i) {
        if (i > 0) os << ", ";
        if constexpr (std::is_same_v<T, std::string>) {
            os << "'" << v[i] << "'";
        } else {
            os << v[i];
        }
    }
    os << "]";
    return os.str();
}

// nested vector to string
template<typename T>
static inline std::string vec_to_str(const std::vector<std::vector<T>>& v) {
    std::ostringstream os;
    os << "[";
    for (size_t i = 0; i < v.size(); ++i) {
        if (i > 0) os << ", ";
        os << vec_to_str(v[i]);
    }
    os << "]";
    return os.str();
}

// stream operator for vectors
template<typename T>
static inline std::ostream& operator<<(std::ostream& os, const std::vector<T>& v) {
    os << vec_to_str(v);
    return os;
}
template<typename K, typename V> static inline std::ostream& operator<<(std::ostream& os, const std::unordered_map<K,V>&){ os << "{...}"; return os; }
template<typename K, typename V> static inline auto py_items(const std::unordered_map<K,V>& m){ std::vector<std::pair<K,V>> v; v.reserve(m.size()); for(const auto& kv: m){ v.emplace_back(kv.first, kv.second);} return v; }


void test_csv_operations() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "=== Testing CSV Operations ===" << std::endl;
    auto data = [;
        std::vector<std::string>{'Name', 'Age', 'City', 'Salary'},;
        std::vector<std::string>{'Alice', '30', 'New York', '75000'},;
        std::vector<std::string>{'Bob', '25', 'San Francisco', '80000'},;
        std::vector<std::string>{'Charlie', '35', 'Chicago', '90000'},;
        std::vector<std::string>{'Diana', '28', 'Boston', '85000'};
    }
    ];
    {
        auto csv_path = f.name;
        auto writer = csv.writer(f);
        writer.writerows(data);
    }
    {
        std::cout << to_str("CSV file created: ") << to_str(csv_path) << std::endl;
        std::cout << "\n--- Reading CSV file ---" << std::endl;
        {
            auto reader = csv.reader(f);
            for (size_t row_num = 0; row_num < reader.size(); ++row_num) {
        auto row = reader[row_num];
                std::cout << to_str("Row ") << to_str(row_num) << to_str(": ") << to_str(row) << std::endl;
            }
        }
        std::cout << "\n--- Reading CSV with DictReader ---" << std::endl;
        {
            auto dict_reader = csv.DictReader(f);
            for (auto row : dict_reader) {
                std::cout << to_str("Person: ") << to_str(row['Name']) << to_str(", Age: ") << to_str(row['Age']) << to_str(", City: ") << to_str(row['City']) << to_str(", Salary: $") << to_str(row['Salary']) << std::endl;
            }
        }
        std::cout << "\n--- Appending to CSV file ---" << std::endl;
        {
            auto writer = csv.writer(f);
            writer.writerow(std::vector<std::string>{'Eve', '32', 'Seattle', '95000'});
        }
        std::cout << "After appending:" << std::endl;
        {
            auto reader = csv.reader(f);
            for (size_t i = 0; i < reader.size(); ++i) {
        auto row = reader[i];
                if (i >= len(data)) {
                    std::cout << to_str("New row: ") << to_str(row) << std::endl;
                }
            }
        }
    }
    {
        if (os.path.exists(csv_path)) {
            os.unlink(csv_path);
        }
    }
}


void test_csv_advanced_features() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing CSV Advanced Features ===" << std::endl;
    auto data = [;
        std::vector<std::string>{'Product', 'Description', 'Price'},;
        std::vector<std::string>{'Laptop', 'High-performance "gaming" laptop', '1299.99'},;
        std::vector<std::string>{'Mouse', 'Wireless, ergonomic design', '29.99'},;
        std::vector<std::string>{'Keyboard', 'Mechanical, RGB backlit', '89.99'};
    }
    ];
    {
        auto csv_path = f.name;
        auto writer = csv.writer(f, delimiter=';', quotechar='"', quoting=csv.QUOTE_MINIMAL);
        writer.writerows(data);
    }
    {
        std::cout << to_str("CSV with custom delimiter created: ") << to_str(csv_path) << std::endl;
        {
            auto content = f.read();
            std::cout << "CSV file content:" << std::endl;
            std::cout << content << std::endl;
        }
        {
            auto reader = csv.reader(f, delimiter=';', quotechar='"');
            for (auto row : reader) {
                std::cout << to_str("Parsed row: ") << to_str(row) << std::endl;
            }
        }
    }
    {
        if (os.path.exists(csv_path)) {
            os.unlink(csv_path);
        }
    }
}


void test_xml_operations() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing XML Operations ===" << std::endl;
    auto root = ET.Element("bookstore");
    auto book1 = ET.SubElement(root, "book");
    book1.set("category", "fiction");
    auto title1 = ET.SubElement(book1, "title");
    title1.text("The Great Gatsby");
    title1.set("lang", "en");
    auto author1 = ET.SubElement(book1, "author");
    author1.text("F. Scott Fitzgerald");
    auto year1 = ET.SubElement(book1, "year");
    year1.text("1925");
    auto price1 = ET.SubElement(book1, "price");
    price1.text("12.99");
    auto book2 = ET.SubElement(root, "book");
    book2.set("category", "science");
    auto title2 = ET.SubElement(book2, "title");
    title2.text("A Brief History of Time");
    title2.set("lang", "en");
    auto author2 = ET.SubElement(book2, "author");
    author2.text("Stephen Hawking");
    auto year2 = ET.SubElement(book2, "year");
    year2.text("1988");
    auto price2 = ET.SubElement(book2, "price");
    price2.text("15.99");
    {
        auto xml_path = f.name;
        auto tree = ET.ElementTree(root);
        tree.write(xml_path, encoding('unicode', xml_declaration=true));
    }
    {
        std::cout << to_str("XML file created: ") << to_str(xml_path) << std::endl;
        std::cout << "\n--- Reading XML file ---" << std::endl;
        auto tree = ET.parse(xml_path);
        auto root = tree.getroot();
        std::cout << "Root element:" << " " << root.tag << std::endl;
        std::cout << "Books in store:" << std::endl;
        for (auto book : root.findall('book')) {
            auto category = book.get('category');
            auto title = book.find('title').text;
            auto author = book.find('author').text;
            auto year = book.find('year').text;
            auto price = book.find('price').text;
            auto lang = book.find('title').get('lang');
            std::cout << to_str("  Category: ") << to_str(category) << std::endl;
            std::cout << to_str("  Title: ") << to_str(title) << to_str(" (") << to_str(lang) << to_str(")") << std::endl;
            std::cout << to_str("  Author: ") << to_str(author) << std::endl;
            std::cout << to_str("  Year: ") << to_str(year) << std::endl;
            std::cout << to_str("  Price: $") << to_str(price) << std::endl;
            std::cout << std::endl;
        }
        std::cout << "--- Modifying XML ---" << std::endl;
        auto new_book = ET.SubElement(root, "book");
        new_book.set("category", "technology");
        auto new_title = ET.SubElement(new_book, "title");
        new_title.text("Clean Code");
        new_title.set("lang", "en");
        auto new_author = ET.SubElement(new_book, "author");
        new_author.text("Robert C. Martin");
        auto new_year = ET.SubElement(new_book, "year");
        new_year.text("2008");
        auto new_price = ET.SubElement(new_book, "price");
        new_price.text("34.99");
        tree.write(xml_path, encoding('unicode', xml_declaration=true));
        std::cout << "XML file updated with new book" << std::endl;
        std::cout << "Updated XML content:" << std::endl;
        {
            std::cout << f.read() << std::endl;
        }
    }
    {
        if (os.path.exists(xml_path)) {
            os.unlink(xml_path);
        }
    }
}


void test_xml_search_and_modify() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing XML Search and Modify ===" << std::endl;
    auto xml_string = ''';
    <library>;
        auto <book id = "1" genre="fiction">;
            <title>Harry Potter</title>;
            <author>J.K. Rowling</author>;
            <year>1997</year>;
            <price>15.99</price>;
        }
        </book>;
        auto <book id = "2" genre="fiction">;
            <title>Lord of the Rings</title>;
            <author>J.R.R. Tolkien</author>;
            <year>1954</year>;
            <price>12.99</price>;
        }
        </book>;
        auto <book id = "3" genre="science">;
            <title>Cosmos</title>;
            <author>Carl Sagan</author>;
            <year>1980</year>;
            <price>18.99</price>;
        }
        </book>;
    }
    </library>;
    /* docstring */
    {
        auto xml_path = f.name;
        f.write(xml_string.strip());
    }
    {
        auto tree = ET.parse(xml_path);
        auto root = tree.getroot();
        std::cout << "Original books:" << std::endl;
        for (auto book : root.findall('book')) {
            auto title = book.find('title').text;
            auto price = book.find('price').text;
            std::cout << to_str("  ") << to_str(title) << to_str(": $") << to_str(price) << std::endl;
        }
        for (auto book : root.findall('book')) {
            auto title = book.find('title').text;
            if ('Harry Potter' in title) {
                book.find('price').text('12.99');
                book.set('discount', 'true');
                std::cout << to_str("Updated price for ") << to_str(title) << std::endl;
            }
        }
        std::cout << "\nBooks by authors with 'J.' in name:" << std::endl;
        for (auto book : root.findall('book')) {
            auto author = book.find('author').text;
            if ('J.' in author) {
                auto title = book.find('title').text;
                std::cout << to_str("  ") << to_str(title) << to_str(" by ") << to_str(author) << std::endl;
            }
        }
        auto new_book = ET.SubElement(root, "book");
        new_book.set("id", "4");
        new_book.set("genre", "technology");
        ET.SubElement(new_book, "title").text("The Pragmatic Programmer");
        ET.SubElement(new_book, "author").text("Andrew Hunt, David Thomas");
        ET.SubElement(new_book, "year").text("1999");
        ET.SubElement(new_book, "price").text("42.99");
        for (auto book : root.findall('book')) {
            if (book.find('title').text == 'Cosmos') {
                root.remove(book);
                std::cout << "Removed Cosmos book" << std::endl;
            }
        }
        tree.write(xml_path, encoding('unicode', xml_declaration=true));
        std::cout << "\nFinal XML:" << std::endl;
        {
            std::cout << f.read() << std::endl;
        }
    }
    {
        if (os.path.exists(xml_path)) {
            os.unlink(xml_path);
        }
    }
}


void test_yaml_like_operations() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing YAML-like Operations (using JSON) ===" << std::endl;
    auto config = {;
        'server': {;
            'host': 'localhost',;
            'port': 8080,;
            'debug': true;
        }
        },;
        'database': {;
            'type': 'postgresql',;
            'host': 'db.example.com',;
            'port': 5432,;
            'credentials': {;
                'username': 'admin',;
                'password': 'secret123';
            }
            };
        }
        },;
        'features': {;
            'logging': {;
                'level': 'info',;
                'file': '/var/log/app.log';
            }
            },;
            'caching': {;
                'enabled': true,;
                'ttl': 3600;
            }
            };
        }
        };
    }
    };
    {
        auto json_path = f.name;
        json.dump(config, f, indent(2));
    }
    {
        std::cout << to_str("Configuration file created: ") << to_str(json_path) << std::endl;
        {
            auto loaded_config = json.load(f);
        }
        std::cout << "Loaded configuration:" << std::endl;
        std::cout << json.dumps(loaded_config, indent=2) << std::endl;
        auto loaded_config['server']['port'] = 9090;
        auto loaded_config['features']['new_feature'] = std::unordered_map<std::string, std::string>{{"enabled", to_str(true)}, {"priority", 'high'}};
        {
            json.dump(loaded_config, f, indent(2));
        }
        std::cout << "\nUpdated configuration:" << std::endl;
        {
            std::cout << f.read() << std::endl;
        }
    }
    {
        if (os.path.exists(json_path)) {
            os.unlink(json_path);
        }
    }
}


int main() {
    /* docstring */
    测试Python的CSV、XML、YAML文件处理;
    /* docstring */
    test_csv_operations();
    test_csv_advanced_features();
    test_xml_operations();
    test_xml_search_and_modify();
    test_yaml_like_operations();
    std::cout << "\n=== All CSV/XML/JSON tests completed ===" << std::endl;
    return 0;
}

