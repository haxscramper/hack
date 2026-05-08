import sys

from PySide6.QtWidgets import QApplication

from ascii_diagram.view.main_window import MainWindow


def main():
    app = QApplication(sys.argv)
    app.setApplicationName("ASCII Diagrammer")
    window = MainWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()