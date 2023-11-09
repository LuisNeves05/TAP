import pj.assessment.{AssessmentMS01, AssessmentMS03}
import pj.io.FileIO

val file = "C:\\Users\\Jorge\\OneDrive\\Ambiente de Trabalho\\tap-2023-base\\files\\assessment\\ms03\\valid00_in.xml"
// Loasdasdsaa2adsaadsaadsadsaddsasaaaadsaadadsasdsaadsddsadsdsasdasdaasadssdsadaadsadasadddsaadsaadsadsaaddsdaafdssaaasaadsaaa dsastadsaadsaaasaddasdasaahe dsaXdssaMdsdsasaaaaL iawsadasddsadasasdsadsdsadasdsaaddsadsadsasanapaadsadsadasuaddsasaaddsasat asddsadsaaasafialadsaeaaaaaaaaaaaaaasasdasaaaaaaaaaaaa
val xmlResult = FileIO.load(file).flatMap(AssessmentMS03.create)