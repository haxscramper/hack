from PyQt6.QtGui import QTextDocument, QTextOption


def create_text_document(
    rich_text: str,
    width: float,
) -> QTextDocument:
    document = QTextDocument()
    document.setDocumentMargin(0.0)

    option = document.defaultTextOption()
    option.setWrapMode(QTextOption.WrapMode.WrapAtWordBoundaryOrAnywhere)
    document.setDefaultTextOption(option)

    document.setHtml(rich_text)
    document.setTextWidth(width)
    document.adjustSize()
    document.setTextWidth(width)
    return document
