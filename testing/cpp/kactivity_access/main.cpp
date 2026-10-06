#include <PlasmaActivities/Consumer>
#include <PlasmaActivities/Info>
#include <PlasmaActivities/Stats/ResultSet>
#include <PlasmaActivities/Stats/Terms>

#include <QCoreApplication>
#include <QJsonArray>
#include <QJsonDocument>
#include <QJsonObject>
#include <QTextStream>

int main(int argc, char* argv[]) {
    QCoreApplication application(argc, argv);

    const QStringList arguments = application.arguments();

    if (arguments.size() != 2) {
        QTextStream(stderr) << "Usage: " << arguments.first() << " ACTIVITY_NAME\n";
        return 1;
    }

    const QString requestedName = arguments.at(1);

    KActivities::Consumer consumer;

    QString activityId;

    for (const QString& candidateId : consumer.activities()) {
        const KActivities::Info info(candidateId);

        if (info.name() != requestedName) { continue; }

        if (!activityId.isEmpty()) {
            QTextStream(stderr)
                << "Multiple activities have the name: " << requestedName << '\n';
            return 1;
        }

        activityId = candidateId;
    }

    if (activityId.isEmpty()) {
        QTextStream(stderr) << "Activity not found: " << requestedName << '\n';
        return 1;
    }

    using namespace KActivities::Stats;
    using namespace KActivities::Stats::Terms;

    Query query = Query::linkedResources();
    query.setActivities(QStringList{activityId});
    query.setTypes(QStringList{QStringLiteral("inode/directory")});

    const ResultSet results(query);

    QJsonArray directories;

    for (const ResultSet::Result& result : results) {
        directories.append(result.resource());
    }

    QJsonObject output;
    output.insert(QStringLiteral("activityId"), activityId);
    output.insert(QStringLiteral("activityName"), requestedName);
    output.insert(QStringLiteral("directories"), directories);

    QTextStream(stdout) << QJsonDocument(output).toJson(QJsonDocument::Indented);

    return 0;
}
