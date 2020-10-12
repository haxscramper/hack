#ifndef COMMON_HPP
#define COMMON_HPP

#include <QDebug>
#include <QElapsedTimer>

#include <string>
#include <vector>

#define let const auto

using strvec = std::vector<std::string>;
using str = std::string;
using score_vec_t = std::vector<std::pair<const std::string, int>>;

inline QElapsedTimer make_timer() {
  QElapsedTimer res;
  res.start();
  return res;
}

inline void logTimer(QElapsedTimer &timer, QString msg) {
  qDebug() << msg << timer.nsecsElapsed() / 1000000 << " msecs";
  timer.restart();
}

#endif // COMMON_HPP
