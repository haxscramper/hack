#pragma once

#include <memory>
#include <vector>

enum class OrgSemKind { None, Text, Paragraph, Subtree };

struct Org {
  std::vector<std::shared_ptr<Org>> subnodes;

  std::shared_ptr<Org> at(int idx);
  virtual OrgSemKind getKind() const = 0;
  virtual ~Org() = default;
};

struct Text : public Org {
  virtual OrgSemKind getKind() const override { return OrgSemKind::Text; }
  virtual ~Text() = default;
  std::string text;
};

struct Paragraph : public Org {
  virtual OrgSemKind getKind() const override { return OrgSemKind::Paragraph; }
  virtual ~Paragraph() = default;
};

struct Subtree : public Org {
  virtual OrgSemKind getKind() const override { return OrgSemKind::Subtree; }
  virtual ~Subtree() = default;
  std::shared_ptr<Paragraph> title;
  int level;
};
