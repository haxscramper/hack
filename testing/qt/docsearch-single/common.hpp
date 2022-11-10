#ifndef COMMON_HPP
#define COMMON_HPP

#include <QDebug>
#include <QElapsedTimer>
#include <QPair>

#include <string>
#include <vector>

//using strvec = QVector<QString>;
//using str = QString;
//using score_vec_t = QVector<QPair<const QString, int>>;

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
