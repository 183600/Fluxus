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


void get_db_connection(int db_path) {
    bool __fluxus_exc=false;
    /* docstring */
    auto conn = sqlite3.connect(db_path);
    conn.row_factory(sqlite3.Row);
    {
        /* yield */
    }
    {
        conn.close();
    }
}


void create_sample_database(int db_path) {
    /* docstring */
    std::cout << "=== Creating Sample Database ===" << std::endl;
    {
        auto cursor = conn.cursor();
        cursor.execute(''';
            CREATE TABLE IF NOT EXISTS users (;
                id INTEGER PRIMARY KEY AUTOINCREMENT,;
                username TEXT UNIQUE NOT NULL,;
                email TEXT UNIQUE NOT NULL,;
                age INTEGER,;
                created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,;
                updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP;
            }
            );
        }
        /* docstring */
        cursor.execute(''';
            CREATE TABLE IF NOT EXISTS orders (;
                id INTEGER PRIMARY KEY AUTOINCREMENT,;
                user_id INTEGER,;
                product_name TEXT NOT NULL,;
                quantity INTEGER NOT NULL,;
                price REAL NOT NULL,;
                order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,;
                status TEXT DEFAULT 'pending',;
                FOREIGN KEY (user_id) REFERENCES users (id);
            }
            );
        }
        /* docstring */
        cursor.execute('CREATE INDEX IF NOT EXISTS idx_users_email ON users (email)');
        cursor.execute('CREATE INDEX IF NOT EXISTS idx_orders_user_id ON orders (user_id)');
        cursor.execute('CREATE INDEX IF NOT EXISTS idx_orders_status ON orders (status)');
        conn.commit();
        std::cout << "✓ Database tables created successfully" << std::endl;
    }
}


void test_basic_crud_operations(int db_path) {
    /* docstring */
    std::cout << "\n=== Testing Basic CRUD Operations ===" << std::endl;
    {
        auto cursor = conn.cursor();
        auto users_data = [;
            ('alice', 'alice@example.com', 25),;
            ('bob', 'bob@example.com', 30),;
            ('charlie', 'charlie@example.com', 35),;
            ('diana', 'diana@example.com', 28);
        }
        ];
        cursor.executemany(;
            'INSERT INTO users (username, email, age) VALUES (?, ?, ?)',;
            users_data;
        }
        );
        auto orders_data = [;
            (1, 'Laptop', 1, 999.99),;
            (1, 'Mouse', 2, 29.99),;
            (2, 'Keyboard', 1, 79.99),;
            (3, 'Monitor', 1, 299.99),;
            (4, 'Headphones', 1, 149.99),;
            (2, 'USB Cable', 3, 19.99);
        }
        ];
        cursor.executemany(;
            'INSERT INTO orders (user_id, product_name, quantity, price) VALUES (?, ?, ?, ?)',;
            orders_data;
        }
        );
        conn.commit();
        std::cout << to_str("✓ Inserted ") << to_str(len(users_data)) << to_str(" users and ") << to_str(len(orders_data)) << to_str(" orders") << std::endl;
        std::cout << "\n--- Reading Data ---" << std::endl;
        cursor.execute('SELECT * FROM users ORDER BY id');
        auto users = cursor.fetchall();
        std::cout << "All users:" << std::endl;
        for (auto user : users) {
            std::cout << to_str("  ID: ") << to_str(user['id']) << to_str(", Username: ") << to_str(user['username']) << to_str(", Email: ") << to_str(user['email']) << to_str(", Age: ") << to_str(user['age']) << std::endl;
        }
        cursor.execute('SELECT * FROM users WHERE username(?', ('alice',)));
        auto alice = cursor.fetchone();
        std::cout << to_str("\\nAlice's details: ") << to_str(dict(alice)) << std::endl;
        cursor.execute('SELECT * FROM users WHERE age > ? ORDER BY age', (28,));
        auto older_users = cursor.fetchall();
        std::cout << to_str("\\nUsers older than 28:") << std::endl;
        for (auto user : older_users) {
            std::cout << to_str("  ") << to_str(user['username']) << to_str(": ") << to_str(user['age']) << to_str(" years old") << std::endl;
        }
        std::cout << "\n--- Updating Data ---" << std::endl;
        cursor.execute('UPDATE users SET age(? WHERE username = ?', (26, 'alice')));
        cursor.execute('UPDATE orders SET status(? WHERE user_id = ?', ('completed', 1)));
        conn.commit();
        cursor.execute('SELECT age FROM users WHERE username(?', ('alice',)));
        auto alice_age = cursor.fetchone()['age'];
        std::cout << to_str("✓ Alice's age updated to: ") << to_str(alice_age) << std::endl;
        std::cout << "\n--- Deleting Data ---" << std::endl;
        cursor.execute('DELETE FROM orders WHERE id(?', (6,)));
        conn.commit();
        cursor.execute('SELECT COUNT(*) as count FROM orders');
        auto order_count = cursor.fetchone()['count'];
        std::cout << to_str("✓ Remaining orders: ") << to_str(order_count) << std::endl;
    }
}


void test_advanced_queries(int db_path) {
    /* docstring */
    std::cout << "\n=== Testing Advanced Queries ===" << std::endl;
    {
        auto cursor = conn.cursor();
        cursor.execute(''';
            SELECT u.username, u.email, o.product_name, o.quantity, o.price, o.status;
            FROM users u;
            JOIN orders o ON u.id(o.user_id);
            ORDER BY u.username, o.order_date;
        }
        /* docstring */
        auto user_orders = cursor.fetchall();
        std::cout << "User orders (JOIN):" << std::endl;
        auto current_user = 0;
        for (auto row : user_orders) {
            if (row['username'] != current_user) {
                auto current_user = row['username'];
                std::cout << to_str("\\n  ") << to_str(current_user) << to_str(" (") << to_str(row['email']) << to_str("):") << std::endl;
            }
            std::cout << to_str("    - ") << to_str(row['product_name']) << to_str(": ") << to_str(row['quantity']) << to_str(" x $") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<row['price']; return os.str(); }()) << to_str(" [") << to_str(row['status']) << to_str("]") << std::endl;
        }
        std::cout << "\n--- Aggregation Functions ---" << std::endl;
        cursor.execute('SELECT COUNT(*) as total_users FROM users');
        auto total_users = cursor.fetchone()['total_users'];
        std::cout << to_str("Total users: ") << to_str(total_users) << std::endl;
        cursor.execute('SELECT AVG(age) as avg_age FROM users');
        auto avg_age = cursor.fetchone()['avg_age'];
        std::cout << to_str("Average user age: ") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(1); os<<avg_age; return os.str(); }()) << std::endl;
        cursor.execute('SELECT SUM(price * quantity) as total_revenue FROM orders');
        auto total_revenue = cursor.fetchone()['total_revenue'];
        std::cout << to_str("Total revenue: $") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<total_revenue; return os.str(); }()) << std::endl;
        cursor.execute('SELECT MAX(price) as max_price FROM orders');
        auto max_price = cursor.fetchone()['max_price'];
        std::cout << to_str("Most expensive order: $") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<max_price; return os.str(); }()) << std::endl;
        cursor.execute(''';
            SELECT u.username, COUNT(o.id) as order_count, SUM(o.price * o.quantity) as total_spent;
            FROM users u;
            LEFT JOIN orders o ON u.id(o.user_id);
            GROUP BY u.id, u.username;
            ORDER BY total_spent DESC;
        }
        /* docstring */
        auto user_stats = cursor.fetchall();
        std::cout << "\nUser spending statistics:" << std::endl;
        for (auto stat : user_stats) {
            std::cout << to_str("  ") << to_str(stat['username']) << to_str(": ") << to_str(stat['order_count']) << to_str(" orders, $") << ([&](){ std::ostringstream os; os.setf(std::ios::fixed); os<<std::setprecision(2); os<<stat['total_spent'] or 0; return os.str(); }()) << to_str(" total") << std::endl;
        }
    }
}


void test_transactions(int db_path) {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Transactions ===" << std::endl;
    std::cout << "--- Successful Transaction ---" << std::endl;
    {
        {
            auto cursor = conn.cursor();
            cursor.execute('BEGIN TRANSACTION');
            cursor.execute('INSERT INTO users (username, email, age) VALUES (?, ?, ?)',;
                        ('eve', 'eve@example.com', 32));
                    }
                }
            }
            cursor.execute('INSERT INTO orders (user_id, product_name, quantity, price) VALUES (?, ?, ?, ?)',;
                        (5, 'Tablet', 1, 399.99));
                    }
                }
            }
            conn.commit();
            std::cout << "✓ Transaction committed successfully" << std::endl;
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Transaction failed: ") << to_str(e) << std::endl;
    }
    std::cout << "\n--- Transaction Rollback ---" << std::endl;
    {
        {
            auto cursor = conn.cursor();
            cursor.execute('BEGIN TRANSACTION');
            cursor.execute('INSERT INTO users (username, email, age) VALUES (?, ?, ?)',;
                        ('frank', 'frank@example.com', 40));
                    }
                }
            }
            cursor.execute('INSERT INTO users (username, email, age) VALUES (?, ?, ?)',;
                        ('frank', 'duplicate@example.com', 25));
                    }
                }
            }
            conn.commit();
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Expected integrity error: ") << to_str(e) << std::endl;
        std::cout << "✓ Transaction was automatically rolled back" << std::endl;
    }
}


void test_prepared_statements(int db_path) {
    /* docstring */
    std::cout << "\n=== Testing Prepared Statements ===" << std::endl;
    {
        auto cursor = conn.cursor();
        auto insert_user = 'INSERT INTO users (username, email, age) VALUES (?, ?, ?)';
        auto new_users = [;
            ('grace', 'grace@example.com', 27),;
            ('henry', 'henry@example.com', 33),;
            ('iris', 'iris@example.com', 29);
        }
        ];
        cursor.executemany(insert_user, new_users);
        conn.commit();
        std::cout << to_str("✓ Inserted ") << to_str(len(new_users)) << to_str(" users using prepared statement") << std::endl;
        auto query_user = 'SELECT * FROM users WHERE age > ? AND age < ?';
        cursor.execute(query_user, (25, 35));
        auto users_in_range = cursor.fetchall();
        std::cout << to_str("Users aged 25-35: ") << to_str(len(users_in_range)) << std::endl;
        for (auto user : users_in_range) {
            std::cout << to_str("  ") << to_str(user['username']) << to_str(": ") << to_str(user['age']) << to_str(" years old") << std::endl;
        }
    }
}


void test_database_metadata(int db_path) {
    /* docstring */
    std::cout << "\n=== Testing Database Metadata ===" << std::endl;
    {
        auto cursor = conn.cursor();
        cursor.execute("SELECT name FROM sqlite_master WHERE type('table'"));
        auto tables = cursor.fetchall();
        std::cout << "Database tables:" << std::endl;
        for (auto table : tables) {
            std::cout << to_str("  - ") << to_str(table['name']) << std::endl;
        }
        for (auto table_name : std::vector<std::string>{'users', 'orders'}) {
            cursor.execute(f"PRAGMA table_info({table_name})");
            auto columns = cursor.fetchall();
            std::cout << to_str("\\n") << to_str(table_name) << to_str(" table structure:") << std::endl;
            for (auto col : columns) {
                std::cout << to_str("  ") << to_str(col['name']) << to_str(": ") << to_str(col['type']) << to_str(" (NULL: ") << to_str(col['notnull'] == 0) << to_str(")") << std::endl;
            }
        }
        cursor.execute('SELECT sqlite_version()');
        auto sqlite_version = cursor.fetchone()[0];
        std::cout << to_str("\\nSQLite version: ") << to_str(sqlite_version) << std::endl;
    }
}


void test_error_handling(int db_path) {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "\n=== Testing Error Handling ===" << std::endl;
    std::cout << "--- Testing Duplicate Key Error ---" << std::endl;
    {
        {
            auto cursor = conn.cursor();
            cursor.execute('INSERT INTO users (username, email, age) VALUES (?, ?, ?)',;
                        ('alice', 'duplicate@example.com', 25));
                    }
                }
            }
            conn.commit();
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Caught expected integrity error: ") << to_str(e) << std::endl;
    }
    std::cout << "\n--- Testing Foreign Key Constraint ---" << std::endl;
    {
        {
            auto cursor = conn.cursor();
            cursor.execute('INSERT INTO orders (user_id, product_name, quantity, price) VALUES (?, ?, ?, ?)',;
                        (999, 'Invalid Product', 1, 99.99));
                    }
                }
            }
            conn.commit();
            std::cout << "Note: Foreign key constraints may not be enforced by default in SQLite" << std::endl;
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("✓ Caught foreign key constraint error: ") << to_str(e) << std::endl;
    }
    std::cout << "\n--- Testing SQL Injection Protection ---" << std::endl;
    auto malicious_input = "'; DROP TABLE users; --";
    {
        {
            auto cursor = conn.cursor();
            cursor.execute('SELECT * FROM users WHERE username(?', (malicious_input,)));
            auto results = cursor.fetchall();
            std::cout << to_str("✓ Parameterized query safely handled malicious input: ") << to_str(len(results)) << to_str(" results") << std::endl;
        }
    }
    if (__fluxus_exc) { auto e = 0;
        std::cout << to_str("Error handling malicious input: ") << to_str(e) << std::endl;
    }
}


int main() {
    bool __fluxus_exc=false;
    /* docstring */
    std::cout << "Python SQLite Integration Demonstration" << std::endl;
    std::cout << std::string(50, '=') << std::endl;
    {
        auto db_path = tmp.name;
    }
    {
        create_sample_database(db_path);
        test_basic_crud_operations(db_path);
        test_advanced_queries(db_path);
        test_transactions(db_path);
        test_prepared_statements(db_path);
        test_database_metadata(db_path);
        test_error_handling(db_path);
        std::cout << to_str("\\n=== All SQLite tests completed successfully ===") << std::endl;
        std::cout << to_str("Database file: ") << to_str(db_path) << std::endl;
        std::cout << "Key concepts covered:" << std::endl;
        std::cout << "- Database creation and table schema" << std::endl;
        std::cout << "- CRUD operations (Create, Read, Update, Delete)" << std::endl;
        std::cout << "- SQL JOIN queries" << std::endl;
        std::cout << "- Aggregation functions (COUNT, SUM, AVG, MAX)" << std::endl;
        std::cout << "- GROUP BY and data grouping" << std::endl;
        std::cout << "- Transaction management (commit/rollback)" << std::endl;
        std::cout << 0 << std::endl;
        std::cout << "- Database metadata inspection" << std::endl;
        std::cout << 0 << std::endl;
    }
    {
        if (os.path.exists(db_path)) {
            os.unlink(db_path);
            std::cout << to_str("\\nCleaned up temporary database: ") << to_str(db_path) << std::endl;
        }
    }
    return 0;
}


