function doGet(e) {
    return HtmlService.createHtmlOutputFromFile('forms.html')
        .setTitle("Google File Upload by CTRLQ.org");
}

function msg(message) {
 Logger.log(message); 
}

function uploadFileToGoogleDrive(data, file, conf) {
    Logger.log(conf);
    try {

        var input = "mirea_input";
        var folder, folders = DriveApp.getFoldersByName(input);
        Logger.log("Uploading to folder" + input);

        if (folders.hasNext()) {
            folder = folders.next();
        } else {
            folder = DriveApp.createFolder(input);
        }

        var contentType = data.substring(5, data.indexOf(';')),
            bytes = Utilities.base64Decode(
                data.substr(data.indexOf('base64,') + 7)),
            blob = Utilities.newBlob(bytes, contentType, file);

        var name = conf.secondname + "_" + conf.name;
        var filename = name + "_" + conf.group + "_Assing_#" +
                       conf.assign + ".pdf";

        Logger.log("Uploading to folder: " + name);
        var file = folder.createFolder(name).createFile(blob);
        file.setName(filename);

        Logger.log("File upload done");
      
      var sheet;
      var files = DriveApp.searchFiles(
        'starred = true and mimeType = "' + MimeType.GOOGLE_SHEETS + '"');
      while (files.hasNext()) {
        var spreadsheet = SpreadsheetApp.open(files.next());
        sheet = spreadsheet.getSheets()[0];
        
        msg(sheet.getName());
        if (sheet.getName() == "assign") {
          msg("found 'assign' stared spreadsheet");
          break; 
        }
       
      }
      
      sheet.appendRow([conf.name + " " + conf.secondname, conf.group, conf.assign]);
      
      
        return "OK";

    } catch (f) {
        Logger.log(f.toString());
        return f.toString();
    }
}
