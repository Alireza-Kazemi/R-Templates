Attribute VB_Name = "Module1"
Sub TightenTextBox()
    Dim shp As Shape
    Dim sld As Slide
    
    ' Check if there is a selected shape
    If Not ActiveWindow.Selection.Type = ppSelectionShapes Then
        MsgBox "Please select a text box.", vbExclamation
        Exit Sub
    End If
    
    ' Loop through each selected shape
    For Each shp In ActiveWindow.Selection.ShapeRange
        ' Check if the selected shape contains text
        If shp.TextFrame.HasText Then
            ' Fit shape to text
            shp.TextFrame.AutoSize = ppAutoSizeShapeToFitText
        Else
            MsgBox "Selected shape does not contain text.", vbExclamation
        End If
    Next shp
End Sub

