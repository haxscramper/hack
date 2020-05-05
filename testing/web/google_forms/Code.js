function doGet(e) {
    return HtmlService.createHtmlOutputFromFile("forms.html")
        .setTitle("Google file upload");
}

function hello() {
    console.log("Hello");
}

function uploadFileToGoogleDrive(data, file, conf) {
    try {
        var folder,
            input = "input",
            folders = DriveApp.getFoldersByName(input);

        Logger.log("Uploading to folder" + input);

        if (folders.hasNext()) {
            folder = folders.next();
        } else {
            folder = DriveApp.createFolder(input);
        }

        var contentType = data.substring(5, data.indexOf(";")),
            bytes = Utilities.base64Decode(
                data.substr(data.indexOf("base64,") + 7)),
            blob = Utilities.newBlob(bytes, contentType, file),
            name = conf.secondname + "_" + conf.name,
            filename = name + "_" + conf.group + "_Assing_#" +
                conf.assign + ".pdf";


        Logger.log("Uploading to folder" + conf.group);


        folder.createFolder(conf.group)
            .createFolder(name)
            .createFile(blob)
            .setName(filename);

        return "OK";
    } catch (f) {
        return f.toString();
    }
}
