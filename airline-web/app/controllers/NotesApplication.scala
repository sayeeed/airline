package controllers

import com.patson.data.NotesSource
import controllers.AuthenticationObject.{Authenticated, AuthenticatedAirline}
import play.api.mvc._
import play.api.libs.json.{JsValue, Json, Writes}
import org.apache.commons.text.StringEscapeUtils

import javax.inject.Inject
import scala.util.{Failure, Success, Try}

class NotesApplication @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

    implicit object AirlineNotesWrites extends Writes[NotesSource.AirlineNotes] {
        def writes(notes: NotesSource.AirlineNotes): JsValue = {
            Json.obj(
                "airlineNotes" -> Json.toJson(notes.airlineNotes.map(StringEscapeUtils.escapeHtml4)),
                "airportNotes" -> Json.toJson(notes.airportNotes.map(StringEscapeUtils.escapeHtml4)),
                "linkNotes" -> Json.toJson(notes.linkNotes.map(StringEscapeUtils.escapeHtml4))
            )
        }
    }

  /**
    * Retrieve all notes for a given airlineId.
    * @param airlineId The airline ID
    * @return JSON response with the list of notes
    */
  def getNotesByAirline(airlineId: Int) = AuthenticatedAirline(airlineId) { request =>
    Try {
      val notes = NotesSource.loadNotesByAirline(airlineId)
      Ok(Json.toJson(notes))
    } match {
      case Success(result) => result
      case Failure(exception) =>
        BadRequest(Json.obj("error" -> s"Failed to retrieve notes: ${exception.getMessage}"))
    }
  }

  /**
    * Endpoint to save a note to the NOTES_AIRLINE_TABLE.
    * @param airlineId The airline ID
    * @param note The note text
    * @return JSON response indicating success or failure
    */
  def saveNoteToAirline(airlineId: Int) = AuthenticatedAirline(airlineId) { request =>
    val note = request.body.asInstanceOf[AnyContentAsJson].json.\("note").as[String]
    val sanitizedNote = StringEscapeUtils.escapeEcmaScript(note)
    Try {
      NotesSource.saveNoteToAirlineTable(airlineId, sanitizedNote)
      Ok(Json.obj("success" -> true))
    } match {
      case Success(result) => result
      case Failure(exception) =>
        BadRequest(Json.obj("error" -> s"Failed to save note: ${exception.getMessage}"))
    }
  }

  /**
    * Endpoint to save a note to the NOTES_AIRPORT_TABLE.
    * @param airlineId The airline ID
    * @param linkId The link ID
    * @param note The note text
    * @return JSON response indicating success or failure
    */
  def saveNoteToAirport(airlineId: Int, linkId: Int) = AuthenticatedAirline(airlineId) { request =>
    val note = request.body.asInstanceOf[AnyContentAsJson].json.\("note").as[String]
    val sanitizedNote = StringEscapeUtils.escapeEcmaScript(note)
    Try {
      NotesSource.saveNoteToAirportTable(linkId, airlineId, sanitizedNote)
      Ok(Json.obj("success" -> true))
    } match {
      case Success(result) => result
      case Failure(exception) =>
        BadRequest(Json.obj("error" -> s"Failed to save note: ${exception.getMessage}"))
    }
  }

  /**
    * Endpoint to save a note to the NOTES_LINK_TABLE.
    * @param airportId The airport ID
    * @param airlineId The airline ID
    * @param note The note text
    * @return JSON response indicating success or failure
    */
  def saveNoteToLink(airlineId: Int, airportId: Int) = AuthenticatedAirline(airlineId) { request =>
    val note = request.body.asInstanceOf[AnyContentAsJson].json.\("note").as[String]
    val sanitizedNote = StringEscapeUtils.escapeEcmaScript(note)
    Try {
      NotesSource.saveNoteToLinkTable(airportId, airlineId, sanitizedNote)
      Ok(Json.obj("success" -> true))
    } match {
      case Success(result) => result
      case Failure(exception) =>
        BadRequest(Json.obj("error" -> s"Failed to save note: ${exception.getMessage}"))
    }
  }
}