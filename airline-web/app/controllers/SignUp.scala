package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import javax.inject._
import views._
import models._
import com.patson.data.UserSource
import com.patson.model._
import com.patson.Authentication

import java.util.Calendar
import com.patson.data.AirlineSource
import com.patson.util.AirlineCache
import play.api.libs.ws.WSClient

import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import play.api.libs.json.Writes
import play.api.libs.json.Json
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import play.api.libs.json.JsNumber
import play.api.libs.json.JsString

class SignUp @Inject()(cc: ControllerComponents)(ws: WSClient) extends AbstractController(cc) with play.api.i18n.I18nSupport {
  private[this] val recaptchaUrl = "https://www.google.com/recaptcha/api/siteverify"
  private[this] val recaptchaAction = "signup"
  private[this] val recaptchaSecret = "6LefN0MnAAAAAF58zLqHe7sDsfJ2q7AFUl1FP9ls"
  private[this] val recaptchaScoreThreshold = 0.5
  val MIN_AIRLINE_NAME_LENGTH = 1
  val MAX_AIRLINE_NAME_LENGTH = 50
  /**
   * Sign Up Form definition.
   *
   * Once defined it handle automatically, ,
   * validation, submission, errors, redisplaying, ...
   */
  val signupForm: Form[NewUser] = Form(
    
    // Define a mapping that will handle User values
    mapping(
      "username" -> text(minLength = 4, maxLength = 20).verifying(
        "username can only contain alphanumeric characters",
        userName => userName.forall(char => char.isLetterOrDigit && char <= 'z')).verifying(
        "This username is not available",
        userName => !UserSource.loadUsersByCriteria(List.empty).map { _.userName.toLowerCase() }.contains(userName.toLowerCase())    
      ),
      "email" -> email,
      // Create a tuple mapping for the password/confirm
      "password" -> tuple(
        "main" -> text(minLength = 6),
        "confirm" -> text
      ).verifying(
        // Add an additional constraint: both passwords must match
        "Passwords don't match", passwords => passwords._1 == passwords._2
      ),
      "recaptchaToken" -> text,
      "airlineName" -> text(minLength = MIN_AIRLINE_NAME_LENGTH, maxLength = MAX_AIRLINE_NAME_LENGTH).verifying(
        "Airline name can only contain space and characters",
        airlineName => airlineName.forall(char => (char.isLetter && char <= 'z')  || char == ' ') && !"".equals(airlineName.trim())).verifying(
        "This airline name is not available",
        airlineName => !AirlineSource.loadAllAirlines(false).map { _.name.toLowerCase().replaceAll("\\s", "") }.contains(airlineName.replaceAll("\\s", "").toLowerCase())
      )
    )
    // The mapping signature doesn't match the User case class signature,
    // so we have to define custom binding/unbinding functions
    {
      // Binding: Create a User from the mapping result (ignore the second password and the accept field)
      (username, email, passwords, recaptureToken, airlineName) => NewUser(username.trim, passwords._1, email.trim, recaptureToken, airlineName.trim)
    } 
    {
      // Unbinding: Create the mapping values from an existing User value
      user => Some(user.username, user.email, (user.password, ""), "", user.airlineName)
    }
  )
  
  /**
   * Display an empty form.
   */
  def form = Action { implicit request =>
    Ok(html.signup(signupForm))
  }
  
  /**
   * Display a form pre-filled with an existing User.
   */
//  def editForm = Action {
//    val existingUser = NewUser("fakeuser", "secret", "fake@gmail.com") 
//    Ok(html.signup(signupForm.fill(existingUser)))
//  }

  def airlineNameCheck(airlineName : String)= Action { implicit request =>
    val length = airlineName.length
    if (length < MIN_AIRLINE_NAME_LENGTH || length > MAX_AIRLINE_NAME_LENGTH) {
      Ok(Json.obj("rejection" -> s"Length should be $MIN_AIRLINE_NAME_LENGTH - $MAX_AIRLINE_NAME_LENGTH"))
    } else if (!airlineName.forall(char => (char.isLetter && char <= 'z') || char == ' ')) {
      Ok(Json.obj("rejection" -> s"Contains invalid character(s)"))
    } else if (AirlineSource.loadAirlinesByCriteria(List(("name", airlineName.trim))).length > 0) {
      Ok(Json.obj("rejection" -> s"Airline name is already taken"))
    } else {
      Ok(Json.obj("ok" -> true))
    }
  }

   /**
   * Handle form submission.
   */
  def submit = Action { implicit request =>
    signupForm.bindFromRequest().fold(
      // Form has errors, redisplay it
      errors => BadRequest(html.signup(errors)), { userInput =>
        
        // if (isValidRecaptcha(userInput.recaptchaToken)) {
          // We got a valid User value, display the summary
          val user = User(userInput.username, userInput.email, Calendar.getInstance, Calendar.getInstance, UserStatus.ACTIVE, level = 0, None, List.empty)
          UserSource.saveUser(user)
          Authentication.createUserSecret(userInput.username, userInput.password)
          
          val newAirline = Airline(userInput.airlineName)
          newAirline.setMinimumRenewalBalance(300000)
          newAirline.setAirlineCode(newAirline.getDefaultAirlineCode())
          AirlineSource.saveAirlines(List(newAirline))
          UserSource.setUserAirline(user, newAirline)

          AirlineSource.saveAirplaneRenewal(newAirline.id, 40)

          SearchUtil.addAirline(newAirline)
          
//          val profile = StartupProfile.profilesById(userInput.profileId)
//          profile.initializeAirline(newAirline)
          Redirect("/").withCookies(Cookie("sessionActive", "true", httpOnly = false)).withSession("userToken" -> SessionUtil.addUserId(user.id))
        // } else {
        //   BadRequest("Recaptcha check failed!")
        // }
        //Ok(html.index("User " + user.userName + " created! Please log in"))
      }
    )
  }
  
  def isValidRecaptcha(recaptchaToken: String) : Boolean = {
    println("checking token " + recaptchaToken)
    val request = ws.url(recaptchaUrl).withQueryStringParameters("secret" -> recaptchaSecret, "response" -> recaptchaToken)
    
    val (successJs, scoreJs, actionJs, responseBody) = Await.result(request.get().map { response =>
      ((response.json \ "success"), (response.json \ "score"), (response.json \ "action"), response.body)
    }, Duration(10, TimeUnit.SECONDS))
    
    if (!successJs.as[Boolean]) {
      println("recaptcha response with success as false")
      return false;  
    }
    
    val score = scoreJs.as[Double]
    val action = actionJs.as[String]
    
    println("recaptcha score " + score + " action " + action)
    
    return action == recaptchaAction && score >= recaptchaScoreThreshold
  }
}
