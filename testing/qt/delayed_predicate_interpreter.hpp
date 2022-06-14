#ifndef DELAYED_PREDICATE_INTERPRETER_HPP
#define DELAYED_PREDICATE_INTERPRETER_HPP

#include <QThread>
#include <QObject>
#include <QVariant>
#include <QMutex>
#include <QList>
#include <QDebug>

struct EvalRequest {
    int id;
};

struct EvalResponse {
    QVariant result;
    int      id;
};

Q_DECLARE_METATYPE(EvalRequest);
Q_DECLARE_METATYPE(EvalResponse);

class AbstractInterpreter : public QThread
{

    Q_OBJECT
    // Bool to stop the thread evalution
    bool finishEvaluation = false;
    // Input text lock
    QMutex lock;
    // Input requests processing
    QList<EvalRequest> evalItems;

  public:
    bool getFinishEvaluation() const { return finishEvaluation; }

    void setFinishEvaluation(bool newFinishEvaluation) {
        finishEvaluation = newFinishEvaluation;
    }

    void enqueue(EvalRequest request) {
        qDebug() << "[A] Enqueuing new request, id" << request.id;
        lock.lock();
        evalItems.append(request);
        lock.unlock();
    }

  signals:
    void evaluationDone(EvalResponse);

  protected:
    QVariant process(EvalRequest req) {
        qDebug() << "[A] Evaluating request with id" << req.id;
        // Simulate evaluation of the predicate function - specific id
        // value is not important for this example so I use it to
        // control return.
        return QVariant(req.id == 0);
    }

    void run() override {
        QList<EvalRequest> requests;
        while (!finishEvaluation) {
            lock.lock();
            requests = evalItems;
            evalItems.clear();
            requests.detach();
            lock.unlock();

            if (!requests.empty()) {
                qDebug() << "[A] Has requests to process";
                for (const auto& item : requests) {
                    QVariant res = process(item);
                    qDebug() << "[A] Emitting evaluation signal";
                    emit evaluationDone(EvalResponse{res, item.id});
                }
            }
        }
    }
};

class Widget : public QObject
{
    Q_OBJECT
  signals:
    void userSetValue(QVariant);

  public:
    bool     isPending = false;
    QVariant value;
    /// Simulate user data input
    void userChange(QVariant val) {
        emit userSetValue(value);
        apiChange(val);
        setIsPending(true);
    }

    /// Regular, unchecked value change - does not trigger any signals and
    /// slots and used to revert change back if value did not match.
    void apiChange(QVariant val) { value = val; }
    void setIsPending(bool newIsPending) {
        qDebug() << "[W] Pending status changed to"
                 << (newIsPending ? "'yes'" : "'no'");
        isPending = newIsPending;
    }
};

class Wizard : public QObject
{
    Q_OBJECT

  private:
    QMap<int, QVariant> original; ///< Original values of widgets before
                                  ///< change.
    int idCounter = 0;

  public:
    Widget widget;
    Wizard() {
        connect(&widget, &Widget::userSetValue, [this](QVariant value) {
            // If user changed value, remember the original state and
            // request validation in the separate thread. When result is
            // returned it will be appropriately handled in the
            // `evaluationDone`.
            this->original[this->idCounter] = value;
            qDebug() << "[Z] wizard signal changed data, requesting check "
                        "for id"
                     << this->idCounter;
            emit requestEvaluation(EvalRequest{this->idCounter});
            ++this->idCounter;
        });
    }

  public slots:
    void evaluationDone(EvalResponse resp) {
        qDebug() << "[Z] Evaluation response recieved" << resp.id;
        // If this is a know evaluation request
        if (original.contains(resp.id)) {
            bool ok = resp.result.toBool();
            if (!ok) {
                // Reset widget value back after then change
                widget.apiChange(original[resp.id]);
                qDebug() << "[Z] Predicate evaluation requests failed, "
                            "reverting change";
            } else {
                qDebug()
                    << "[Z] Evaluation result ok, not reverint anything";
            }
            // Reset widget 'pending' status regardless, it is either
            // correct (not pending) or had been reset back (also not
            // pending).
            widget.setIsPending(false);
        }
    }

  signals:
    void requestEvaluation(EvalRequest);
};


#endif // DELAYED_PREDICATE_INTERPRETER_HPP
