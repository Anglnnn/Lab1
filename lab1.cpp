#include <algorithm>
#include <vector>
#include <cstdint>
#include <iostream>
#include <fstream>

using namespace std;
typedef int32_t value;

struct power {
    char variable;
    value degree;
};

struct monomial {
    value coefficient;
    vector< power > product;
};

struct polynomial {
    vector< monomial > sum;
};

istream& operator >> (istream& is, power& obj) {

    istream::sentry s(is); // пропускаємо пробіли

    // читаємо символ для змінної
    if (is && isalpha(is.peek())) {
        is >> obj.variable;
    }
    else {
        // інакше, невірне введення
        is.clear(ios::failbit);
    }

    // читаємо степінь
    if (is) {
        if (is.peek() == '^') {
            is.ignore();
            is >> obj.degree;
        }
        else {
            obj.degree = 1;
            is.clear();
        }
    }
    return is;
}
// перегружаємо оператор вивода
ostream& operator << (ostream& os, power const& obj) {
    os << obj.variable;
    if (obj.degree != 1) {
        os << '^' << obj.degree;
    }
    return os;
}

// перегружаємо оператор введення із консолі istream ostream
istream& operator >> (istream& is, monomial& obj) {
    obj.coefficient = 1;
    obj.product.clear();

    // Читання послідовності чисел та зведених у ступінь змінних,
    // необов'язковий роздільник *
    bool did_read_asterisk = false;

    do {
        // читаємо коеф. (ігноруємо пробіл)
        value coefficient;
        if (is >> coefficient) {
            obj.coefficient *= coefficient;
        }
        else if (is.rdstate() & ios::failbit) {
            // якщо коеф. не було - продовжуємо читати
            is.clear(is.rdstate() & ~ios::failbit);

            // читаємо ступінь
            power p;
            if (is >> p) {
                obj.product.push_back(p);
            }

            // Нічого страшного, якщо ступені також не було, якщо тільки не було *
            if (!did_read_asterisk && (is.rdstate() & ios::failbit)) {
                is.clear(is.rdstate() & ~ios::failbit);
                return is;
            }
        }
        did_read_asterisk = false;

        // пропуск пробілів
        if (is >> ws) {
            if (is.eof()) {
                return is;
            }
            if (is.peek() == '*') {
                is.ignore();
                did_read_asterisk = true;
            }
            if (is.peek() == '+' || is.peek() == '-') {
                break;
            }
        }
    } while (is);

    return is;
}
ostream& operator << (ostream& os, monomial const& obj) {
    if (obj.coefficient != 1 || obj.product.empty()) {
        os << obj.coefficient;
    }
    for (power const& p : obj.product) {
        os << p;
    }
    return os;
}
istream& operator >> (istream& is, polynomial& obj) {
    // Пропустити початкову прогалину і відхилити кінець введення
    istream::sentry s(is);

    // Якщо знака мінус немає, починаємо з додатнього 
    bool positive = true;
    if (is && is.peek() == '-') {
        is.ignore();
        positive = false;
    }

    // Читаємо послідовність значень, розділених + або -
    monomial m;
    while (is >> m) {
        if (!positive) m.coefficient = -m.coefficient;
        obj.sum.push_back(m);

        is >> ws;
        char next_op = is.peek();
        if (is && (next_op == '+' || next_op == '-')) {
            is.ignore();
            positive = next_op == '+';

        }
        else if (!is.bad()) {
            // Якщо не знайшли оператор - вдало 
            is.clear();
            return is;
        }
    }
    return is;
}
ostream& operator << (ostream& os, polynomial const& obj) {
    bool skip_leading_plus = true;

    for (monomial const& m : obj.sum) {
        if (m.coefficient > 0 && !skip_leading_plus) {
            os << '+';
        }
        os << m;
        skip_leading_plus = false;
    }
    return os;
}

// перегрузка операторів для вивода в файл ofstream
ofstream& operator << (ofstream& os, polynomial const& obj) {
    bool skip_leading_plus = true;

    for (monomial const& m : obj.sum) {
        if (m.coefficient > 0 && !skip_leading_plus) {
            os << '+';
        }
        os << m;
        skip_leading_plus = false;
    }
    return os;
}

ofstream& operator << (ofstream& os, monomial const& obj) {
    if (obj.coefficient != 1 || obj.product.empty()) {
        os << obj.coefficient;
    }
    for (power const& p : obj.product) {
        os << p;
    }
    return os;
}

ofstream& operator << (ofstream& os, power const& obj) {
    os << obj.variable;
    if (obj.degree != 1) {
        os << '^' << obj.degree;
    }
    return os;
}


struct variable_order {
    bool operator() (power lhs, power rhs) {
        return lhs.variable < rhs.variable;
    }
};
struct variable_same {
    bool operator() (power lhs, power rhs) {
        return lhs.variable == rhs.variable;
    }
};
// основна логіка
monomial simplify(monomial in) {
    sort(in.product.begin(), in.product.end(), variable_order{}); // сортуємо monomial від початку до кінця в порядку важливості
    for (auto it = in.product.begin();
        (it = adjacent_find(it, in.product.end(), variable_same{}))
        != in.product.end(); ) {
        value degree = it->degree;
        it = in.product.erase(it);
        it->degree += degree;
    }
    in.product.erase(remove_if(in.product.begin(), in.product.end(),
        [](power p) { return p.degree == 0; }), in.product.end());
    return in;
}

struct power_order {
    bool operator() (power lhs, power rhs) {
        return lhs.variable < rhs.variable ? true
            : lhs.variable > rhs.variable ? false
            : lhs.degree < rhs.degree;
    }
};
struct power_same {
    bool operator() (power lhs, power rhs) {
        return lhs.variable == rhs.variable
            && lhs.degree == rhs.degree;
    }
};

struct product_order {
    bool operator() (monomial lhs, monomial rhs) {
        return lexicographical_compare(lhs.product.begin(), lhs.product.end(),
            rhs.product.begin(), rhs.product.end(),
            power_order{});
    }
};
struct product_same {
    bool operator() (monomial lhs, monomial rhs) {
        return equal(lhs.product.begin(), lhs.product.end(),
            rhs.product.begin(), rhs.product.end(),
            power_same{});
    }
};

polynomial simplify(polynomial in) {
    for (auto& m : in.sum) {
        m = simplify(m);
    }
    sort(in.sum.begin(), in.sum.end(), product_order{});
    for (auto it = in.sum.begin();
        (it = adjacent_find(it, in.sum.end(), product_same{}))
        != in.sum.end(); ) {
        value coefficient = it->coefficient;
        it = in.sum.erase(it);
        it->coefficient += coefficient;
    }
    in.sum.erase(remove_if(in.sum.begin(), in.sum.end(),
        [](monomial m) { return m.coefficient == 0; }), in.sum.end());

    // Якщо введення невдале - пишемо 0 
    if (in.sum.empty()) in = polynomial{ { monomial{ 0, {} } } };

    return in;
}

int main() {
    setlocale(LC_ALL, "ukr");
    ifstream fin;
    ofstream fout;
    string filename;
    polynomial p;
    int input;
    while (true) {

        cout << "1. Ввести полiном" << endl;
        cout << "2. Спростити полiном" << endl;
        cout << "3. Показати приведенний полiном" << endl;
        cout << "4. Читати полiном iз файла" << endl;
        cout << "5. Зберегти результати в файл" << endl;
        cout << "6. Вихiд" << endl;
        cout << "\tВиберiть функцiю: ";
        cin >> input;
        switch (input)
        {
        case 1:
            cout << "Вводьте полiном iз змiнних(знак = означає завершеннiсть введення):" << endl;
            cin.clear();
            cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            cin >> p;
            cin.clear();
            cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
            break;
        case 2:
            cout << "Полiном спрощений (введiть 3, щоб подивитись результат!)" << endl;
            p = simplify(p);
            break;
        case 3:
            cout << "\tСпрощений полiном: " << p << endl;
            break;
        case 4:
            cout << "Введiть назву файла .txt iз полiномом: "; cin >> filename;
            fin.open(filename);
            if (fin.is_open()) {
                fin >> p;
                cout << "\tЗчитаний полiном iз файла: " << p << endl;
            }
            else {
                cout << "\tНеможливо знайти файл " << filename << "!" << endl;
            }
            break;
        case 5:
            cout << "Введiть назву файла .txt для збереження даних: "; cin >> filename;
            fout.open(filename, ios::app);
            if (fout.is_open()) {
                fout << p << endl << endl;
                cout << "\tПоліном " << p << " збережений в файл " << filename << endl;
            }
            else {
                cout << "\tНеможливо знайти файл " << filename << "!" << endl;
            }
            break;
        case 6:
            return 0;
        default:
            break;
        }


    }

}