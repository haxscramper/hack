#!/usr/bin/env python

# copied from
# https://stackoverflow.com/questions/44385030/inserting-an-image-in-a-qscintilla-editor
# https://web.archive.org/web/20190604145015/https://qscintilla.com/insert-images-basics/

import sys
from PyQt5.QtWidgets import *
from PyQt5.QtCore import *
from PyQt5.QtGui import *
from PyQt5.Qsci import *

class ImageScintilla(QsciScintilla):
    """
    Specialized QScintilla editor that allows insertion and deletion
    of images into the editor itself.
    """

    class Image:
        """
        Class for holding an image's information
        """

        def __init__(self, image, position, size=None):
            if isinstance(image, str) == False:
                raise Exception("Enter path to image as a string.")
            elif isinstance(position, tuple) == False or len(position) != 2:
                raise Exception("Image position should be of type tuple(int, int)!")
            elif size != None and (isinstance(position, tuple) == False \
                                           or len(position) != 2):
                raise Exception("Image size has to be of type tuple(int, int)!")
            # Assign the properties
            self.image = QImage(image)
            if size != None:
                self.image = self.image.scaled(*size)
            self.position = position
            self.size = size
    ''''''

    maximum_image_count = 100
    image_list = None
    calculation_font = None
    line_list = None

    def __init__(self, parent=None):
        super().__init__(parent)
        self.image_list = {}
        self.line_list = []
        # Connect the special SCN_MODIFIED signal, that gives details of
        # changes in the editor's text.
        self.SCN_MODIFIED.connect(self.text_changed)
    ''''''

    def set_calculation_font(self, font):
        """
        Set the font for calculating the font metrics like font width and height
        Parameters:
            font -> QFont, a QFont that will be used for the calculations
        """
        if isinstance(font, QFont) == False:
            raise Exception("The calculation font has to be a QFont!")
        self.calculation_font = font
    ''''''

    def add_image(self, image, position, size):
        """
        Add the image to the image list.
        Parameters:
            image -> str, path to image
            position -> tuple(int, int), the COLUMN and LINE offset for the image position
            size -> tuple(int, int), the height and width of the displayed image.
                    If None, the original image size will be used.
        Return value:
            index -> int, index of the added image
        """
        image = self.Image(image, position, size)
        for i in range(self.maximum_image_count):
            if not (i in self.image_list.keys()):
                self.image_list[i] = image
                return i
        else:
            raise Exception("Too many images in the editor, the maximum is '{}'"\
                            .format(self.maximum_image_count))
    ''''''

    def delete_image(self, index):
        """
        Delete an image from the image list using it's index number
        Parameters:
            index -> int, the index of the image
        """
        if isinstance(index, int) == False or index > self.maximum_image_count:
            raise Exception(
                "Index for deletion should be smaller integer than maximum_image_count")
        # Delete the image from the image list by
        # poping the entry out of the dictionary!
        self.image_list.pop(index, None)
    ''''''

    def get_font_metrics(self, font):
        font_metrics = QFontMetrics(font)
        single_character_width = font_metrics.width("A")  # Works for monospaced fonts!
        # For other fonts it is a good enough estimate.
        single_character_height = font_metrics.height()
        return single_character_width, single_character_height
    ''''''

    def text_changed(self,
                     position,
                     mod_type,
                     text,
                     length,
                     lines_added,
                     line,
                     fold_level_now,
                     fold_level_prev,
                     token,
                     additional_lines_added):
        """
        This is a function connected to the SCN_MODIFIED signal.
        It gives great information of what changes happened in the editor.
        """
        insert_flag = mod_type & 0b1
        delete_flag = mod_type & 0b10
        if insert_flag or delete_flag:

            change_line, change_column = self.lineIndexFromPosition(position)
            #            print(change_line, lines_added)
            print("---")
            print("text:", text)
            print("length:", length)
            print("change line:", change_line, "change column:", change_column)
            if lines_added != 0:
                # Loop through the lines and adjust the Y(LINE) position offset
                for key, image in self.image_list.items():
                    x, y = image.position
                    if y >= change_line:
                        image.position = (x, y + lines_added)

            for key, image in self.image_list.items():
                col, row  = image.position
                change_row = change_line - 1
                if row == change_row:
                    if insert_flag and change_column < col:
                        print("inserted text on line", row, ", adjusting image")
                        if insert_flag:
                            image.position = (col + length, row)
                        elif delete_flag:
                            image.position = (col - length, row)
                else:
                    print(f"Image on row {row} is ignored - change row is {change_row}")

    ''''''

    def paintEvent(self, e):
        super().paintEvent(e)
        # Get the painting frame details
        current_parent_size = self.size()
        first_visible_line = self.SendScintilla(self.SCI_GETFIRSTVISIBLELINE)
        column_offset_in_pixels = self.SendScintilla(self.SCI_GETXOFFSET)
        single_character_width, single_character_height = self.get_font_metrics(
            self.calculation_font
        )
        # Initialize the painter
        painter = QPainter()
        painter.begin(self.viewport())
        # Loop throught the images and paint them
        for i in self.image_list.keys():
            # Set the paint offsets
            image = self.image_list[i].image
            paint_offset_x = (self.image_list[i].position[0] * single_character_width)\
                             - column_offset_in_pixels
            paint_offset_y = (self.image_list[i].position[1] - first_visible_line)\
                             * single_character_height
            # Paint the image
            painter.drawImage(QPoint(paint_offset_x, paint_offset_y), image)
        # Close the painter
        painter.end()
    ''''''

#========================  create editor window  =========================#


myCodeSample = r"""#include <stdio.h>

/*
 * I want an image
 * right here =>
 */

int main()
{
    char arr[5] = {'h', 'e', 'l', 'l', 'o'};

    int i;

    for(i = 0; i < 5; i++) {
        printf(arr[i]);
    }

    return 0;

}

/*
 * Now another test image here =>
 */

/*
 * And another test image here, a bit farther in =>
 */

/*
 * !!! A deleted image will not be shown here !!! =>
 */
""".replace("\n", "\r\n")


class CustomMainWindow(QMainWindow):
    def __init__(self):
        super(CustomMainWindow, self).__init__()

        # Window setup
        # --------------

        # 1. Define the geometry of the main window
        self.setGeometry(300, 300, 800, 400)
        self.setWindowTitle("QScintilla Test")

        # 2. Create frame and layout
        self.__frm = QFrame(self)
        self.__frm.setStyleSheet("QWidget { background-color: #ffeaeaea }")
        self.__lyt = QVBoxLayout()
        self.__frm.setLayout(self.__lyt)
        self.setCentralWidget(self.__frm)
        self.__myFont = QFont("Consolas", 14, weight=QFont.Bold)
        self.__myFont.setPointSize(14)

        # 3. Place a button
        self.__btn = QPushButton("Qsci")
        self.__btn.setFixedWidth(50)
        self.__btn.setFixedHeight(50)
        self.__btn.clicked.connect(self.__btn_action)
        self.__btn.setFont(self.__myFont)
        self.__lyt.addWidget(self.__btn)

        # QScintilla editor setup
        # ------------------------

        # 1. Make instance of ImageScintilla class
        # (instead of QsciScintilla)
        self.__editor = ImageScintilla()
        self.__editor.setText(myCodeSample)
        self.__editor.setLexer(None)
        self.__editor.setUtf8(True)  # Set encoding to UTF-8
        self.__editor.setFont(self.__myFont)  # Can be overridden by lexer

        # 2. Image tests
        self.__editor.set_calculation_font(self.__myFont)

        img = [None, None, None, None]
        img[0] = self.__editor.add_image("nim_logo.png", (19, 4),  (30, 30))
        img[1] = self.__editor.add_image("nim_logo.png", (35, 21), (90, 75))
        img[2] = self.__editor.add_image("nim_logo.png",       (53, 25), (85, 85))
        img[3] = self.__editor.add_image("nim_logo.png", (55, 30), (60, 60))
        self.__editor.delete_image(img[3])

        # 3. Add editor to layout
        self.__lyt.addWidget(self.__editor)

        self.show()

    ''''''

    def __btn_action(self):
        print("Hello World!")

    ''''''


''' End Class '''

if __name__ == '__main__':
    app = QApplication(sys.argv)
    QApplication.setStyle(QStyleFactory.create('Fusion'))
    myGUI = CustomMainWindow()

    sys.exit(app.exec_())

''''''
