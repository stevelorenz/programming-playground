#include <boost/intrusive/list.hpp>
#include <iostream>
#include <string>
#include <utility>

struct animal : public boost::intrusive::list_base_hook<> {
	std::string name;
	int legs;
	animal(std::string n, int l) : name{std::move(n)}, legs{l} {}
};

int main() {
	animal a1{"cat", 4};
	animal a2{"shark", 0};
	animal a3{"spider", 8};
	typedef boost::intrusive::list<animal> animal_list;
	animal_list animals;

	animals.push_back(a1);
	animals.push_back(a2);
	animals.push_back(a3);

	for (const auto &a : animals) {
		std::cout << a.name << std::endl;
	}
}
