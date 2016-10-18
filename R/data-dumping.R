#' Gastric Dumping
#' 
#' A contingency table relating surgical operation, centre and severity of
#' gastric dumping, a syndrome associated with gastric surgery.
#' 
#' Gastric dumping syndrome is a condition where ingested foods bypass the
#' stomach too rapidly and enter the small intestine largely undigested. It is
#' an undesirable side-effect of gastric surgery. The table summarizes the
#' results of a study comparing four different surgical operations on patients
#' with duodenal ulcer, carried out in four centres, as described in Grizzle et
#' al (1969). The four operations were: vagotomy and drainage, vagotomy and
#' antrectomy (removal of 25\% of gastric tissue), vagotomy and hemigastrectomy
#' (removal of 50\% of gastric tissue), and gastric restriction (removal of
#' 75\% of gastric tissue).
#' 
#' @name dumping
#' @docType data
#' @format A 3x4x4 table of counts cross-classified by Symptom
#' (none/slight/moderate), Operation (Vd/Va/Vh/Gr) and Centre (1:4).
#' @source Grizzle JE, Starmer CF, Koch GG (1969) Analysis of categorical data
#' by linear models. Biometrics 25(3):489-504.
#' @keywords datasets
#' @examples
#' 
#' data(dumping) 
#' plot(dumping) 
#' 
"dumping"
