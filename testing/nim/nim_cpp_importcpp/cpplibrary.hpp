#include <string>

class Parent
{
  public:
// Interface method to override
  virtual std::string getName() = 0;
};

class Default : public Parent
{
public:
  virtual std::string getName() override {
    return "name for default implementation";
  }
};

int getInt(int arg) {
    return arg + 12;
}
