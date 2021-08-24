/*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2016 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

/**
   {3 GitHub APIv3 client library}

   This library offers thin but natural bindings to
   {{:https://docs.github.com/rest}GitHub's developer API}.
*/;

/** Modules of this type are returned from the {!Github_core.Make}
    functor which may be applied to Cohttp_lwt client libraries in
    order to run on Unix, in a browser in Javascript, or as a MirageOS
    unikernel. */

module type Github = {
  /** {4 API Concepts} */;

  /** [Message] may be raised by any API call when the GitHub service
      returns an unexpected response code. Typical reasons for this
      exception are insufficient permissions or missing resources. */

  exception Message(Cohttp.Code.status_code, Github_t.message);

  /** Functions corresponding to direct API requests return
      {!Response.t} values inside of {!Monad.t} values so that more
      information about the request can be made
      available. {!Monad.(>>~)} is a convenience operator that lets
      you bind directly to the carried value. */

  module Response: {
    /** [redirect] indicates whether the originally requested
        endpoint should continue to be used in the future. */

    type redirect =
      | /** The redirection is temporary. */
        Temporary(Uri.t)
      | /** The redirection is permanent. */
        Permanent(Uri.t);

    /** ['a t] is an API response containing a payload of type
        ['a]. {b Do not} refer to this type explicitly as its identity and
        representation are subject to change (e.g. a family of object
        types may replace it before 3.0). */

    type t('a) =
      pri {
        ..
        value: 'a,
        redirects: list(redirect),
      };

    /** [value r] is the payload in response [r]. */

    let value: {.. value: 'a} => 'a;

    /** [redirects r] is the sequence of redirects prior to response [r]. */

    let redirects: {.. redirects: list(redirect)} => list(redirect);

    /** [final_resource rs] is the single redirect, if any redirects
        occurred, that describes the overall redirect chain [rs]. If
        any redirect [rs] is temporary, [final_resource rs] will be a
        temporary redirect to the final URI. If all redirects [rs] are
        permanent, [final_resource rs] will be a permanent redirect to
        the final URI. */

    let final_resource: list(redirect) => option(redirect);
  };

  /** All API requests are bound through this monad which encapsulates
      an Lwt cooperative thread and includes some state which may be
      set via {!API} functions. */

  module Monad: {
    /** ['a t] is an Lwt thread sensitive to GitHub API state. */

    type t('a);

    /** [return x] is the value [x] wrapped in a state-sensitive Lwt thread. */

    let return: 'a => t('a);

    /** [bind m f] is the eventual value of [f] applied to the
        contents of [m]. Its argument order is designed for currying. */

    let bind: ('a => t('b), t('a)) => t('b);

    /** [map f m] is {!bind} [m (fun x -> return (f x))]. Its argument
        order is designed for currying. */

    let map: ('a => 'b, t('a)) => t('b);

    /** [m >>= f] is [{!bind} f m]. */

    let (>>=): (t('a), 'a => t('b)) => t('b);

    /** [m >|= f] is [{!map} f m]. */

    let (>|=): (t('a), 'a => 'b) => t('b);

    /** [m >>~ f] is [m >|= {!Response.value} >>= f]. */

    let (>>~): (t(Response.t('a)), 'a => t('b)) => t('b);

    /** [catch try with] is the result of trying [try]. If [try]
        succeeds, its result is returned. If [try] raises an
        exception, [with] is applied to the exception and the result
        of [with] is returned. */

    let catch: (unit => t('a), exn => t('a)) => t('a);

    /** [fail exn] raises exception [exn] inside of the monad. */

    let fail: exn => t('a);

    /** [run m] is the Lwt thread corresponding to the sequence of API
        actions represented by [m]. Once a {!t} has been [run], any
        GitHub API state such as associated default security tokens or
        declared user agent string will not be available in
        subsequently bound functions. */

    let run: t('a) => Lwt.t('a);

    /** [embed lwt] is an Lwt thread lifted into the GitHub API
        monad. Its monadic state will be inherited from any monadic
        values bound before it. */

    let embed: Lwt.t('a) => t('a);
  };

  /** Each request to GitHub is made to a specific [Endpoint] in
      GitHub's REST-like API. */

  module Endpoint: {
    /** Some endpoints expose resources which change over time and
        responses from those endpoints may contain version metadata
        which can be used to make low-cost conditional requests
        (e.g. cache validation). */

    module Version: {
      /** [t] is a version of a resource representation. */

      type t =
        | /** An entity tag identifier */
          Etag(string)
        | /** A timestamp conforming to the HTTP-date production */
          Last_modified(
            string,
          );
    };
  };

  /** The [Stream] module provides an abstraction to GitHub's paginated
      endpoints. Stream creation does not initiate any network
      activity. When requests are made, results are buffered
      internally. Streams are not mutable. */

  module Stream: {
    /** ['a t] is a stream consisting roughly of a buffer and a means
        to refill it. */

    type t('a);

    /** ['a parse] is the type of functions which extract elements
        from a paginated response. */

    type parse('a) = string => Lwt.t(list('a));

    /** [next s] is the next element of the stream and a stream
        continuation if one exists. The input stream is not
        modified. This function offers an efficient, lazy, and uniform
        means to iterate over ordered API results which may be too
        numerous to fit into a single request/response pair. */

    let next: t('a) => Monad.t(option(('a, t('a))));

    /** [map f s] is the lazy stream of [f] applied to elements of [s]
        as they are demanded. */

    let map: ('a => Monad.t(list('b)), t('a)) => t('b);

    /** [fold f a s] is the left fold of [f] over the elements of [s]
        with a base value of [a]. {b Warning:} this function may
        result in {i many} successive API transactions. */

    let fold: (('a, 'b) => Monad.t('a), 'a, t('b)) => Monad.t('a);

    /** [find p s] is the first value in [s] satisfying [p] if one
        exists and a stream continuation for further ingestion. */

    let find: ('a => bool, t('a)) => Monad.t(option(('a, t('a))));

    /** [iter f s] is activated after the application of [f] to each
        element of [s]. */

    let iter: ('a => Monad.t(unit), t('a)) => Monad.t(unit);

    /** [to_list s] is a list with each element of [s]. {b Warning:}
        this function may result in {i many} successive API transactions. */

    let to_list: t('a) => Monad.t(list('a));

    /** [of_list l] is a stream with each element of [l].
        Occasionally, it is useful to write interfaces which operate
        generically on streams. [of_list] allows you to use list
        values with those interfaces. */

    let of_list: list('a) => t('a);

    /** [poll stream] is a stream with items newer than [stream]'s
        items and will not resolve until any timeouts indicated by
        GitHub have elapsed. By default, GitHub throttles polling
        requests to once per minute per URL endpoint. */

    let poll: t('a) => Monad.t(option(t('a)));

    /** [since stream version] is [stream] with [version] but without
        any other change, i.e. the stream is not reset to its
        beginning. Used in conjunction with [poll], [since] enables
        low-cost conditional re-synchronization of local state with
        GitHub state. */

    let since: (t('a), Endpoint.Version.t) => t('a);

    /** [version stream] is the version of [stream] if one is
        known. After any stream element is forced, the stream version
        will be available unless GitHub violates its API specification. */

    let version: t('a) => option(Endpoint.Version.t);
  };

  /** [rate] is a type used to indicate which
      {{:https://docs.github.com/rest/overview/resources-in-the-rest-api#rate-limiting}rate-limiting
      regime} is to be used for query quota accounting. [rate] is used by the
      function in {!API}. */

  type rate =
    | Core
    | Search; /***/

  /** Some results may require 2-factor authentication. [Result]
      values do not. [Two_factor] values contain the mode by which a
      2FA code will be delivered. This code is required as [?otp] to
      a subsequent invocation of the function which returns this
      type. */

  type authorization('a) =
    | Result('a)
    | Two_factor(string); /***/

  /** ['a parse] is the type of functions which extract meaningful
      values from GitHub responses. */

  type parse(+'a) = string => Lwt.t('a);

  /** ['a handler] is the type of response handlers which consist of
      an activation predicate (fst) and a parse function (snd). */

  type handler('a) = (((Cohttp.Response.t, string)) => bool, 'a);

  /** [log_active] regulates debug messages. It is [true] by default
      when the environment variable [GITHUB_DEBUG] is set to 1. */

  let log_active: ref(bool);

  /** The [Scope] module abstracts GitHub's
      {{:https://docs.github.com/developers/apps/scopes-for-oauth-apps#available-scopes}authorization
      scopes}. */

  module Scope: {
    /** [to_string scope] is the string GitHub uses to indicate
        the scope constructor [scope]. */

    let to_string: Github_t.scope => string;

    /** [scope_of_string scope] is the constructor corresponding to
        the GitHub scope constructor [scope] if one exists. */

    let of_string: string => option(Github_t.scope);

    /** [string_of_scopes scopes] is the serialization for a list of
        scopes [scopes] which GitHub accepts as a set of scopes in its
        API. */

    let list_to_string: list(Github_t.scope) => string;

    /** [scopes_of_string scopes] are the scope constructors
        corresponding to the serialized list of constructors
        [scopes]. */

    let list_of_string: string => option(list(Github_t.scope));

    /** [all] is a list containing every scope constructor known. */

    let all: list(Github_t.scope);

    /** [max] is a list containing the mimimum scope constructors
        needed to enable full privilege. */

    let max: list(Github_t.scope);
  };

  /** The [Token] module manipulates authorization tokens. GitHub has
      two types of tokens:
      {{:https://docs.github.com/developers/apps/authorizing-oauth-apps}OAuth
      application tokens} and
      {{:https://docs.github.com/github/authenticating-to-github/creating-a-personal-access-token}
      "personal tokens"}.

      Note: the OAuth Authorizations API has been deprecated by GitHub.

      @see <https://docs.github.com/rest/reference/oauth-authorizations> OAuth
      Authorizations API
      @see <https://developer.github.com/changes/2019-11-05-deprecated-passwords-and-authorizations-api/#deprecating-and-adding-endpoints-for-the-oauth-authorizations-and-oauth-applications>
      for the OAuth Authorizations deprecation.
  */

  module Token: {
    /** [t] is the abstract type of a token. */

    type t;

    /** [of_code ~client_id ~client_secret ~code ()] is the {!t}
        granted by a [code] from an
        {{:https://docs.github.com/developers/apps/authorizing-oauth-apps#web-application-flow}
        OAuth web flow redirect}. */

    let of_code:
      (~client_id: string, ~client_secret: string, ~code: string, unit) =>
      Lwt.t(option(t));

    /** [create ?otp ~user ~pass ()] is a new authorization with the
        provided fields. When a user has enabled two-factor
        authentication, the return value will be a {!constructor:Two_factor}
        constructor with the one-time password delivery
        mode. Including a valid [?otp] will yield a {!constructor:Result} return
        value. */

    let create:
      (
        ~scopes: list(Github_t.scope)=?,
        ~note: string=?,
        ~note_url: string=?,
        ~client_id: string=?,
        ~client_secret: string=?,
        ~fingerprint: string=?,
        ~otp: string=?,
        ~user: string,
        ~pass: string,
        unit
      ) =>
      Monad.t(Response.t(authorization(Github_t.auth)));

    /** [get_all ~user ~pass ()] are all of the authorizations that
        this user has made. See {!create} for an explanation of how
        two-factor authentication is handled. */

    let get_all:
      (~otp: string=?, ~user: string, ~pass: string, unit) =>
      Monad.t(Response.t(authorization(Github_t.auths)));

    /** [get ~user ~pass ~id ()] is the authorization with identifier
        [id]. See {!create} for an explanation of how two-factor
        authentication is handled. */

    let get:
      (~otp: string=?, ~user: string, ~pass: string, ~id: int64, unit) =>
      Monad.t(Response.t(authorization(option(Github_t.auth))));

    /** [delete ~user ~pass ~id ()] is [Result ()] after the
        authorization with identifier [id] has been removed. See
        {!create} for an explanation of how two-factor authentication
        is handled. */

    let delete:
      (~otp: string=?, ~user: string, ~pass: string, ~id: int64, unit) =>
      Monad.t(Response.t(authorization(unit)));

    /** [of_auth auth] is the OAuth application or personal token
        contained within [auth]. */

    let of_auth: Github_t.auth => t;

    /** [of_string token_string] is the abstract token value
        corresponding to the string [token_string]. */

    let of_string: string => t;

    /** [to_string token] is the string serialization of [token]. */

    let to_string: t => string;
  };

  /** The [API] module contains functionality that relates to the
      entirety of the GitHub API and these bindings. In particular,
      this module contains:

      - {{:https://docs.github.com/rest/overview/resources-in-the-rest-api#http-verbs}generic accessor functions},
        not normally used directly, but useful if you wish to invoke
        API endpoints not yet bound.
      - handler constructors to help with using the generic accessors
      - monad state injectors for setting things like default tokens or
        user agent strings
      - cached, rate limit queries
      - error message utility functions
  */

  module API: {
    /** [code_handler ~expected_code parse] is a response handler that
        fires for responses with status [expected_code] and applies
        [parse]. */

    let code_handler:
      (~expected_code: Cohttp.Code.status_code, 'a) => handler('a);

    /** [get ?rate ?fail_handlers ?expected_code ?headers ?token
        ?params uri p] is the [p]-parsed response to a GitHub API HTTP
        GET request to [uri] with extra query parameters [params] and
        extra headers [headers]. If [token] is supplied, it will be
        used instead of any token bound into the monad. [p] will only
        fire if the response status is [expected_code] which defaults
        to [200 OK]. If the response status is not [expected_code],
        [fail_handlers], if any, will be checked in the order
        supplied. The [rate] parameter determines which rate limit
        accounting regime will be used for caching rate limit values
        in response headers. */

    let get:
      (
        ~rate: rate=?,
        ~fail_handlers: list(handler(parse('a)))=?,
        ~expected_code: Cohttp.Code.status_code=?,
        ~media_type: string=?,
        ~headers: Cohttp.Header.t=?,
        ~token: Token.t=?,
        ~params: list((string, string))=?,
        ~uri: Uri.t,
        parse('a)
      ) =>
      Monad.t(Response.t('a));

    /** [get_stream uri stream_p] is the {!Stream.t} encapsulating
        lazy [stream_p]-parsed responses to GitHub API HTTP GET
        requests to [uri] and
        {{:https://docs.github.com/rest/overview/resources-in-the-rest-api#pagination}its
        successors}. For an explanation of the other
        parameters, see {!get}. */

    let get_stream:
      (
        ~rate: rate=?,
        ~fail_handlers: list(handler(Stream.parse('a)))=?,
        ~expected_code: Cohttp.Code.status_code=?,
        ~media_type: string=?,
        ~headers: Cohttp.Header.t=?,
        ~token: Token.t=?,
        ~params: list((string, string))=?,
        ~uri: Uri.t,
        Stream.parse('a)
      ) =>
      Stream.t('a);

    /** [post uri p] is the [p]-parsed response to a GitHub API HTTP
        POST request to [uri]. For an explanation of the other
        parameters, see {!get}. */

    let post:
      (
        ~rate: rate=?,
        ~fail_handlers: list(handler(parse('a)))=?,
        ~expected_code: Cohttp.Code.status_code,
        ~headers: Cohttp.Header.t=?,
        ~body: string=?,
        ~token: Token.t=?,
        ~params: list((string, string))=?,
        ~uri: Uri.t,
        parse('a)
      ) =>
      Monad.t(Response.t('a));

    /** [delete uri p] is the [p]-parsed response to a GitHub API HTTP
        DELETE request to [uri]. For an explanation of the other
        parameters, see {!get}. */

    let delete:
      (
        ~rate: rate=?,
        ~fail_handlers: list(handler(parse('a)))=?,
        ~expected_code: Cohttp.Code.status_code=?,
        ~headers: Cohttp.Header.t=?,
        ~token: Token.t=?,
        ~params: list((string, string))=?,
        ~uri: Uri.t,
        parse('a)
      ) =>
      Monad.t(Response.t('a));

    /** [patch uri p] is the [p]-parsed response to a GitHub API HTTP
        PATCH request to [uri]. For an explanation of the other
        parameters, see {!get}. */

    let patch:
      (
        ~rate: rate=?,
        ~fail_handlers: list(handler(parse('a)))=?,
        ~expected_code: Cohttp.Code.status_code,
        ~headers: Cohttp.Header.t=?,
        ~body: string=?,
        ~token: Token.t=?,
        ~params: list((string, string))=?,
        ~uri: Uri.t,
        parse('a)
      ) =>
      Monad.t(Response.t('a));

    /** [put uri p] is the [p]-parsed response to a GitHub API HTTP
        PUT request to [uri]. For an explanation of the other
        parameters, see {!get}. */

    let put:
      (
        ~rate: rate=?,
        ~fail_handlers: list(handler(parse('a)))=?,
        ~expected_code: Cohttp.Code.status_code,
        ~headers: Cohttp.Header.t=?,
        ~body: string=?,
        ~token: Token.t=?,
        ~params: list((string, string))=?,
        ~uri: Uri.t,
        parse('a)
      ) =>
      Monad.t(Response.t('a));

    /** [set_user_agent ua] contains monadic state that will cause
        bound requests to use the [User-Agent] header value of [ua]. */

    let set_user_agent: string => Monad.t(unit);

    /** [set_token token] contains monadic state that will cause bound
        requests to use [token] for authentication by default. This
        function enables the creation of large, generic monadic
        compositions that do not have to be parameterized by
        authentication token. */

    let set_token: Token.t => Monad.t(unit);

    /** [get_rate ?rate ()] is the, possibly cached, rate limit information for
        the rate limit regime [?rate] (default {!constructor:Core}). */

    let get_rate:
      (~rate: rate=?, ~token: Token.t=?, unit) => Monad.t(Github_t.rate);

    /** [get_rate_limit ()] is the, possibly cached, {!constructor:Core} total
        request quota for the current token. */

    let get_rate_limit: (~token: Token.t=?, unit) => Monad.t(int);

    /** [get_rate_remaining ()] is the, possibly cached, {!constructor:Core}
        remaining request quota for the current token. */

    let get_rate_remaining: (~token: Token.t=?, unit) => Monad.t(int);

    /** [get_rate_reset ()] is the, possibly cached, {!constructor:Core} UNIX
        epoch expiry time (s) when the remaining request quota will be
        reset to the total request quota for the current token. */

    let get_rate_reset: (~token: Token.t=?, unit) => Monad.t(float);

    /** [string_of_message message] is the English language error
        message that GitHub generated in [message]. */

    let string_of_message: Github_t.message => string;
  };

  /** The [URI] module contains URI generation functions which may be
      useful for linking on the Web or passing to other GitHub API
      clients. */

  module URI: {
    /** The API endpoint for creating and retrieving authorizations. */

    let authorizations: Uri.t;

    /** [authorize ?scopes ?redirect_uri ~client_id ~state ()] is the
        URL to
        {{:https://docs.github.com/developers/apps/authorizing-oauth-apps#redirect-urls}
        redirect users} to in an OAuth2 flow to create an authorization
        token. [?redirect_url] is the URL in your Web application
        where users will be sent after authorization. If omitted, it
        will default to the callback URL in GitHub's OAuth application
        settings. The [state] parameter should match the callback
        state parameter in order to protect against CSRF. */

    let authorize:
      (
        ~scopes: list(Github_t.scope)=?,
        ~redirect_uri: Uri.t=?,
        ~client_id: string,
        ~state: string,
        unit
      ) =>
      Uri.t;

    /** [token ~client_id ~client_secret ~code ()] is the API endpoint
        used by {!Token.of_code} to finish the OAuth2 web flow and
        convert a temporary OAuth code into a real API access token. */

    let token:
      (~client_id: string, ~client_secret: string, ~code: string, unit) =>
      Uri.t;
  };

  /** The [Filter] module contains types used by search and
      enumeration interfaces which describe ways to perform result
      filtering directly in the GitHub API. */

  module Filter: {
    /** [state] is the activation state of a pull request, milestone,
        or issue. See {!Pull.for_repo}, {!Milestone.for_repo}, and
        {!Issue.for_repo}. */

    type state = [ | `All | `Open | `Closed];

    /** [milestone_sort] is the field by which to sort a collection of
        milestones. See {!Milestone.for_repo}. */

    type milestone_sort = [ | `Due_date | `Completeness];

    /** [issue_sort] is the field by which to sort a collection of
        issues. See {!Issue.for_repo}. */

    type issue_sort = [ | `Created | `Updated | `Comments];

    /** [issue_comment_sort] is the field by which to sort a collection of
        issue comments. See {!Issue.comments_for_repo}. */

    type issue_comment_sort = [ | `Created | `Updated];

    /** [repo_sort] is the field by which to sort a collection of
        repositories. See {!Search.repos}. */

    type repo_sort = [ | `Stars | `Forks | `Updated];

    /** [forks_sort] is the bias used when sorting a collection of
        forks. See {!Repo.forks}. */

    type forks_sort = [ | `Newest | `Oldest | `Stargazers];

    /** [direction] is the sortation precedence. */

    type direction = [ | `Asc | `Desc];

    /** [milestone] is the filter predicate for issues. See
        {!Issue.for_repo}. */

    type milestone = [ | `Any | `None | `Num(int)];

    /** [user] is the filter predicate for issues. See
        {!Issue.for_repo}. */

    type user = [ | `Any | `None | `Login(string)];

    /** ['a range] is the type of range expressions in search
        queries. [`Range] is inclusive. See {!qualifier}. */

    type range('a) = [
      | `Range(option('a), option('a))
      | `Lt('a)
      | `Lte('a)
      | `Eq('a)
      | `Gte('a)
      | `Gt('a)
    ];

    /** [repo_field] is a repository search field selector. See [`In]
        in {!qualifier}. */

    type repo_field = [ | `Name | `Description | `Readme];

    /** [date] is the YYYY-MM-DD representation of a day. */

    type date = string;

    /** [issue_qualifier] is the type of issue search query predicates. */

    type issue_qualifier = [
      | `Author(string)
      | `Assignee(string)
      | `Mentions(string)
      | `Commenter(string)
      | `Involves(string)
      | `Team(string)
      | `Label(string)
      | `Without_label(string)
      | `Language(string)
      | `Created(range(date))
      | `Updated(range(date))
      | `Merged(range(date))
      | `Closed(range(date))
      | `User(string)
      | `Repo(string)
      | `Project(string)
    ];

    /** [qualifier] is the type of repository search query predicates. */

    type qualifier = [
      | `In(list(repo_field))
      | `Size(range(int))
      | `Stars(range(int))
      | `Forks(range(int))
      | `Fork([ | `True | `Only])
      | `Created(range(date))
      | `Pushed(range(date))
      | `User(string)
      | `Language(string)
    ];
  };

  /** {4 API Modules} */;

  /** The [Rate_limit] module contains explicit rate limit API request
      functions which do not {i read} the rate limit cache but do {i
      write} to it. */

  module Rate_limit: {
    /** [all ()] is the current token's rate limit information for all
        rate limiting regimes. */

    let all: (~token: Token.t=?, unit) => Monad.t(Github_t.rate_resources);

    /** [for_core ()] is the current token's rate limit information
        for the {!constructor:Core} rate limit regime. */

    let for_core: (~token: Token.t=?, unit) => Monad.t(Github_t.rate);

    /** [for_search ()] is the current token's rate limit information
        for the {!constructor:Search} rate limit regime. */

    let for_search: (~token: Token.t=?, unit) => Monad.t(Github_t.rate);
  };

  /** The [User] module provides basic user information query functions. */

  module User: {
    /** [current_info ()] is the user information linked to the
        current token. */

    let current_info:
      (~token: Token.t=?, unit) => Monad.t(Response.t(Github_t.user_info));

    /** [info ~user ()] is the user information for user [user]. */

    let info:
      (~token: Token.t=?, ~user: string, unit) =>
      Monad.t(Response.t(Github_t.user_info));

    /** [repositories ~user ()] is a stream of user [user]'s repositories. */

    let repositories:
      (~token: Token.t=?, ~user: string, unit) =>
      Stream.t(Github_t.repository);
  };

  /** The [Organization] module exposes the functionality of the
      GitHub {{:https://docs.github.com/rest/reference/orgs}organization
      API}. */

  module Organization: {
    /** [teams ~org ()] is a stream of teams belonging to the
        organization [org]. */

    let teams:
      (~token: Token.t=?, ~org: string, unit) => Stream.t(Github_t.team);

    /** [user_orgs ~user ()] is a stream of the organizations
         to which the user [user] belongs. */

    let user_orgs:
      (~token: Token.t=?, ~user: string, unit) => Stream.t(Github_t.org);

    /** [current_user ()] is a stream of the organizations to which
        the user linked to current token belongs, and for which the user
        granted access to the organizations to the current token. */

    let current_user_orgs:
      (~token: Token.t=?, unit) => Stream.t(Github_t.org);

    /** [repositories ~org ()] is a stream of repositories belonging to the
        organization [org]. */

    let repositories:
      (~token: Token.t=?, ~org: string, unit) => Stream.t(Github_t.repository);

    /** The [Hook] module provides access to GitHub's
        {{:https://docs.github.com/rest/reference/orgs#webhooks}organization
        webhooks API} which lets you manage an organization's
        remote notification hooks. */

    module Hook: {
      /** [for_org ~org ()] is a stream of hooks for the organization [org]. */

      let for_org:
        (~token: Token.t=?, ~org: string, unit) => Stream.t(Github_t.hook);

      /** [get ~org ~id ()] is hook [id] for organization [org]. */

      let get:
        (~token: Token.t=?, ~org: string, ~id: int64, unit) =>
        Monad.t(Response.t(Github_t.hook));

      /** [create ~org ~hook ()] is a newly created post-receive
          hook for organization [org] as described by [hook]. */

      let create:
        (~token: Token.t=?, ~org: string, ~hook: Github_t.new_hook, unit) =>
        Monad.t(Response.t(Github_t.hook));

      /** [update ~org ~id ~hook ()] is the updated hook [id] for
          organization [org] as described by [hook]. */

      let update:
        (
          ~token: Token.t=?,
          ~org: string,
          ~id: int64,
          ~hook: Github_t.update_hook,
          unit
        ) =>
        Monad.t(Response.t(Github_t.hook));

      /** [delete ~org ~id ()] activates after hook [id] in
          organization [org] has been deleted. */

      let delete:
        (~token: Token.t=?, ~org: string, ~id: int64, unit) =>
        Monad.t(Response.t(unit));

      /** [test ~org ~id ()] activates after a [push] event for
          the lastest push for organization [org] has been synthesized and
          sent to hook [id]. */

      let test:
        (~token: Token.t=?, ~org: string, ~id: int64, unit) =>
        Monad.t(Response.t(unit));

      /** [parse_event ~constr ~payload ()] is the event with
            constructor [constr] that is represented by [payload]. */

      let parse_event:
        (~constr: string, ~payload: string, unit) => Github_t.event_hook_constr;

      /** [parse_event_metadata ~payload ()] is the event metadata for
            the serialized event [payload]. */

      let parse_event_metadata:
        (~payload: string, unit) => Github_t.event_hook_metadata;
    };
  };

  /** The [Team] module contains functionality relating to GitHub's
      {{:https://docs.github.com/rest/reference/teams}team API}. */

  module Team: {
    /** [info ~id ()] is a description of team [id]. */

    let info:
      (~token: Token.t=?, ~id: int64, unit) =>
      Monad.t(Response.t(Github_t.team_info));

    /** [repositories ~id ()] is a stream of repositories belonging
        to team [id]. */

    let repositories:
      (~token: Token.t=?, ~id: int64, unit) => Stream.t(Github_t.repository);
  };

  /** The [Event] module exposes GitHub's
      {{:https://docs.github.com/rest/reference/activity#events}event API}
      functionality. */

  module Event: {
    /** [for_repo ~user ~repo ()] is a stream of all events for
        [user]/[repo]. */

    let for_repo:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.event);

    /** [public_events ()] is a stream of all public events on GitHub. */

    let public_events: unit => Stream.t(Github_t.event);

    /** [for_network ~user ~repo ()] is a stream of all events for the
        fork network containing [user]/[repo]. */

    let for_network:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.event);

    /** [for_org ~org ()] is a stream of all events for the
        organization [org]. */

    let for_org:
      (~token: Token.t=?, ~org: string, unit) => Stream.t(Github_t.event);

    /** [for_org_member ~user ~org ()] is a stream of [org] events
        which [user] receives. */

    let for_org_member:
      (~token: Token.t=?, ~user: string, ~org: string, unit) =>
      Stream.t(Github_t.event);

    /** [received_by_user ~user ()] is a stream of all of the events
        [user] receives. If the current token is for [user], public
        and private events will be returned. If not, only public
        events will be returned. */

    let received_by_user:
      (~token: Token.t=?, ~user: string, unit) => Stream.t(Github_t.event);

    /** [received_by_user_public ~user ()] is a stream of the public
        events [user] receives. */

    let received_by_user_public:
      (~token: Token.t=?, ~user: string, unit) => Stream.t(Github_t.event);

    /** [for_user ~user ()] is a stream of the events generated by
        [user]. If the current token is for [user], public and private
        events will be returned. If not, only public events will be
        returned. */

    let for_user:
      (~token: Token.t=?, ~user: string, unit) => Stream.t(Github_t.event);

    /** [for_user_public ~user ()] is a stream of the public events
        generated by [user]. */

    let for_user_public:
      (~token: Token.t=?, ~user: string, unit) => Stream.t(Github_t.event);
  };

  /** The [Repo] module offers the functionality of GitHub's
      {{:https://docs.github.com/rest/reference/repos}repository API}. */

  module Repo: {
    /** [create ?organization new_repo ()] is a new repository owned
        by the user or organizations if it's provided. */

    let create:
      (
        ~token: Token.t=?,
        ~organization: string=?,
        ~repo: Github_t.new_repo,
        unit
      ) =>
      Monad.t(Response.t(Github_t.repository));

    /** [info ~user ~repo ()] is a description of repository [user]/[repo]. */

    let info:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Monad.t(Response.t(Github_t.repository));

    /** [fork ?organization ~user ~repo ()] is a newly forked
        repository from [user]/[repo] to the current token's user or
        [organization] if it's provided. */

    let fork:
      (
        ~token: Token.t=?,
        ~organization: string=?,
        ~user: string,
        ~repo: string,
        unit
      ) =>
      Monad.t(Response.t(Github_t.repository));

    /** [forks ?sort ~user ~repo ()] is a stream of all repositories
        forked from [user]/[repo] sorted by [?sort] (default [`Newest]). */

    let forks:
      (
        ~token: Token.t=?,
        ~sort: Filter.forks_sort=?,
        ~user: string,
        ~repo: string,
        unit
      ) =>
      Stream.t(Github_t.repository);

    /** [get_tag ~user ~repo ~sha ()] is the annotated tag object with
        SHA [sha] in [user]/[repo]. */

    let get_tag:
      (~token: Token.t=?, ~user: string, ~repo: string, ~sha: string, unit) =>
      Monad.t(Response.t(Github_t.tag));

    /** [tags ~user ~repo ()] is a stream of all tags in repo [user]/[repo]. */

    let tags:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.repo_tag);

    /** [get_tags_and_times ~user ~repo ()] is a stream of pairs of
        tag names and creation times for all lightweight and annotated
        tags in [user]/[repo]. */

    let get_tags_and_times:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t((string, string));

    /** [branches ~user ~repo ()] is a stream of all branches in repo
        [user]/[repo]. */

    let branches:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.repo_branch);

    /** [refs ?ty ~user ~repo ()] is a stream of all
        {{:https://docs.github.com/rest/reference/git#references}git references}
        with prefix [?ty] for repo [user]/[repo]. */

    let refs:
      (~token: Token.t=?, ~ty: string=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.git_ref);

    /** [get_ref ~user ~repo ~name] is the
        {{:https://docs.github.com/rest/reference/git#references}git reference}
        with name [name] for repo [user]/[repo]. */

    let get_ref:
      (~token: Token.t=?, ~user: string, ~repo: string, ~name: string, unit) =>
      Monad.t(Response.t(Github_t.git_ref));

    /** [get_commits ~user ~repo ()] stream of all commits in [user]/[repo]. */

    let get_commits:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.commit);

    /** [get_commit ~user ~repo ~sha ()] is commit [sha] in [user]/[repo]. */

    let get_commit:
      (~token: Token.t=?, ~user: string, ~repo: string, ~sha: string, unit) =>
      Monad.t(Response.t(Github_t.commit));

    /** [contributors ~user ~repo ()] is a stream of contributors to
        repo [user]/[repo]. */

    let contributors:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.contributor);

    /** [delete ~user ~repo ()] activates after repo [user]/[repo] has
        been deleted. */

    let delete:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Monad.t(Response.t(unit));

    /** The [Hook] module provides access to GitHub's
        {{:https://docs.github.com/rest/reference/repos#webhooks}webhooks API}
        which lets you manage a repository's post-receive hooks. */

    module Hook: {
      /** [for_repo ~user ~repo ()] is a stream of hooks for repo
          [user]/[repo]. */

      let for_repo:
        (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
        Stream.t(Github_t.hook);

      /** [get ~user ~repo ~id ()] is hook [id] for repo [user]/[repo]. */

      let get:
        (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
        Monad.t(Response.t(Github_t.hook));

      /** [create ~user ~repo ~hook ()] is a newly created post-receive
          hook for repo [user]/[repo] as described by [hook]. */

      let create:
        (
          ~token: Token.t=?,
          ~user: string,
          ~repo: string,
          ~hook: Github_t.new_hook,
          unit
        ) =>
        Monad.t(Response.t(Github_t.hook));

      /** [update ~user ~repo ~id ~hook ()] is the updated hook [id] in
          repo [user]/[repo] as described by [hook]. */

      let update:
        (
          ~token: Token.t=?,
          ~user: string,
          ~repo: string,
          ~id: int64,
          ~hook: Github_t.update_hook,
          unit
        ) =>
        Monad.t(Response.t(Github_t.hook));

      /** [delete ~user ~repo ~id ()] activates after hook [id] in repo
          [user]/[repo] has been deleted. */

      let delete:
        (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
        Monad.t(Response.t(unit));

      /** [test ~user ~repo ~id ()] activates after a [push] event for
          the lastest push to [user]/[repo] has been synthesized and
          sent to hook [id]. */

      let test:
        (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
        Monad.t(Response.t(unit));

      /** [parse_event ~constr ~payload ()] is the event with
            constructor [constr] that is represented by [payload]. */

      let parse_event:
        (~constr: string, ~payload: string, unit) => Github_t.event_hook_constr;

      /** [parse_event_metadata ~payload ()] is the event metadata for
            the serialized event [payload]. */

      let parse_event_metadata:
        (~payload: string, unit) => Github_t.event_hook_metadata;
    };
  };

  /** The [Stats] module exposes the functionality of GitHub's
      {{:https://docs.github.com/rest/reference/repos#statistics}repository
      statistics API} which provides historical data regarding the
      aggregate behavior of a repository. */

  module Stats: {
    /** [contributors ~user ~repo ()] is a stream of all contributor
        statistics for [user]/[repo]. The stream is empty if the
        data are not cached yet. */

    let contributors:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.contributor_stats);

    /** [yearly_commit_activity ~user ~repo ()] returns the last year of commit
        activity grouped by week for [user]/[repo]. The days array is a group of
        commits per day, starting on Sunday. The stream is empty if the data are
        not cached yet. */

    let yearly_commit_activity:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.commit_activity);

    /** [weekly_commit_activity ~user ~repo ()] returns a weekly aggregate of
        the number of additions and deletions pushed to [user]/[repo]. The
        stream is empty if the data are not cached yet. */

    let weekly_commit_activity:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.code_frequency);

    /** [weekly_commit_count ~user ~repo ()] returns the total commit counts for
        the owner and total commit counts in all. all is everyone combined,
        including the owner in the last 52 weeks. If you'd like to get the
        commit counts for non-owners, you can subtract owner from all.

        The array order is oldest week (index 0) to most recent week.*/

    let weekly_commit_count:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Monad.t(Response.t(Github_t.participation));

    /** [hourly_commit_count ~user ~repo ()] returns the hourly commit count for
        each day.
        Each array contains the day number, hour number, and number of commits:
         - 0-6: Sunday - Saturday
         - 0-23: Hour of day
         - Number of commits
        For example, [2, 14, 25] indicates that there were 25 total commits,
        during the 2:00pm hour on Tuesdays. All times are based on the time
        zone of individual commits.*/

    let hourly_commit_count:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.punch_card);
  };

  /** The [Status] module provides the functionality of GitHub's
      {{:https://docs.github.com/rest/reference/repos#statuses}status API}. */

  module Status: {
    /** [for_sha ~user ~repo ~git_ref ()] is a stream of statuses
        attached to the SHA, branch name, or tag name [git_ref] in
        repo [user]/[repo]. */

    let for_ref:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~git_ref: string,
        unit
      ) =>
      Stream.t(Github_t.status);

    /** [create ~user ~repo ~sha ~status ()] is a newly created status
        on SHA [sha] in repo [user]/[repo] as described by [status]. */

    let create:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~sha: string,
        ~status: Github_t.new_status,
        unit
      ) =>
      Monad.t(Response.t(Github_t.status));

    /** [get ~user ~repo ~sha ()] is the combined status of the ref
        [sha] in the repo [user]/[repo]. */

    let get:
      (~token: Token.t=?, ~user: string, ~repo: string, ~sha: string, unit) =>
      Monad.t(Response.t(Github_t.combined_status));
  };

  /** The [Pull] module contains functionality relating to GitHub's
      {{:https://docs.github.com/rest/reference/pulls}pull request API}. */

  module Pull: {
    /** [for_repo ?state ~user ~repo ()] is a stream of pull requests
        against repo [user]/[repo] which are currently in state
        [?state] (default [`Open]). */

    let for_repo:
      (
        ~token: Token.t=?,
        ~state: Filter.state=?,
        ~user: string,
        ~repo: string,
        unit
      ) =>
      Stream.t(Github_t.pull);

    /** [get ~user ~repo ~num ()] is the pull request [user]/[repo]#[num]. */

    let get:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Monad.t(Response.t(Github_t.pull));

    /** [create ~user ~repo ~pull ()] is the newly created pull
        request against repo [user]/[repo] as described by [pull]. */

    let create:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~pull: Github_t.new_pull,
        unit
      ) =>
      Monad.t(Response.t(Github_t.pull));

    /** [create_from_issue ~user ~repo ~pull_issue ()] is the newly
        created pull request from an issue against repo [user]/[repo]
        as described by [pull_issue]. */

    let create_from_issue:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~pull_issue: Github_t.new_pull_issue,
        unit
      ) =>
      Monad.t(Response.t(Github_t.pull));

    /** [update ~user ~repo ~update_pull ~num ()] is the updated pull
        request [user]/[repo]#[num] as described by [update_pull]. */

    let update:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~update_pull: Github_t.update_pull,
        ~num: int,
        unit
      ) =>
      Monad.t(Response.t(Github_t.pull));

    /** [commits ~user ~repo ~num ()] is the stream of commits
        included in pull request [user]/[repo]#[num]. */

    let commits:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Stream.t(Github_t.commit);

    /** [files ~user ~repo ~num ()] is the stream of files
        included in pull request [user]/[repo]#[num]. */

    let files:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Stream.t(Github_t.file);

    /** [is_merged ~user ~repo ~num ()] is [true] if pull request
        [user]/[repo]#[num] has been merged. */

    let is_merged:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Monad.t(Response.t(bool));

    /** [merge ~user ~repo ~num ?merge_commit_message ()] is the merge
        of pull request [user]/[repo]#[num] with optional commit
        message [?merge_commit_message]. */

    let merge:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~num: int,
        ~merge_commit_message: string=?,
        unit
      ) =>
      Monad.t(Response.t(Github_t.merge));
  };

  /** The [Issue] module gives users access to GitHub's
      {{:https://docs.github.com/rest/reference/issues}issue API}. */

  module Issue: {
    /** [for_repo ?creator ?mentioned ?assignee ?labels ?milestone
        ?state ?sort ?direction ~user ~repo ()] is a stream of issues
        in repo [user]/[repo] which were created by user [?creator],
        mention user [?mentioned], are assigned to user [?assignee],
        have labels [?labels], are included in milestone [?milestone],
        and are in state [?state]. The stream is sorted by [?sort]
        (default [`Created]) and ordered by [?direction] (default
        [`Desc]). */

    let for_repo:
      (
        ~token: Token.t=?,
        ~creator: string=?,
        ~mentioned: string=?,
        ~assignee: Filter.user=?,
        ~labels: list(string)=?,
        ~milestone: Filter.milestone=?,
        ~state: Filter.state=?,
        ~sort: Filter.issue_sort=?,
        ~direction: Filter.direction=?,
        ~user: string,
        ~repo: string,
        unit
      ) =>
      Stream.t(Github_t.issue);

    /** [get ~user ~repo ~num ()] is the issue [user]/[repo]#[num]. */

    let get:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Monad.t(Response.t(Github_t.issue));

    /** [create ~user ~repo ~issue ()] is a newly created issue
        described by [issue] in repo [user]/[repo]. */

    let create:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~issue: Github_t.new_issue,
        unit
      ) =>
      Monad.t(Response.t(Github_t.issue));

    /** [update ~user ~repo ~num ~issue ()] is the updated issue [num]
        in [user]/[repo] as described by [issue]. */

    let update:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~num: int,
        ~issue: Github_t.update_issue,
        unit
      ) =>
      Monad.t(Response.t(Github_t.issue));

    /** [events_for_repo ~user ~repo ()] is a stream of all issue
        events for [user]/[repo]. */

    let events_for_repo:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.repo_issues_event);

    /** [events ~user ~repo ~num ()] is a stream of all issue events
        for [user]/[repo]#[num]. */

    let events:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Stream.t(Github_t.repo_issue_event);

    /** [timeline_events ~user ~repo ~num ()] is a stream of all timeline
        events for [user]/[repo]#[num]. */

    let timeline_events:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Stream.t(Github_t.timeline_event);

    /** [comments ?since ~user ~repo ~num ()] is a stream of issue
        comments for [user]/[repo]#[num]. If [?since], an ISO 8601
        format timestamp (YYYY-MM-DDTHH:MM:SSZ), is supplied, only
        comments updated at or after this time are returned. */

    let comments:
      (
        ~token: Token.t=?,
        ~since: string=?,
        ~user: string,
        ~repo: string,
        ~num: int,
        unit
      ) =>
      Stream.t(Github_t.issue_comment);

    /** [comments_for_repo ~user ~repo ()] is a stream of issue
        comments for repo [user]/[repo] sorted by [?sort] in
        [?direction] order and having occurred since ISO 8601
        timestamp (YYYY-MM-DDTHH:MM:SSZ) [?since]. */

    let comments_for_repo:
      (
        ~token: Token.t=?,
        ~sort: Filter.issue_comment_sort=?,
        ~direction: Filter.direction=?,
        ~since: string=?,
        ~user: string,
        ~repo: string,
        unit
      ) =>
      Stream.t(Github_t.issue_comment);

    /** [create_comment ~user ~repo ~num ~body ()] is a newly created
        issue comment on [user]/[repo]#[num] with content [body]. */

    let create_comment:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~num: int,
        ~body: string,
        unit
      ) =>
      Monad.t(Response.t(Github_t.issue_comment));

    /** [get_comment ~user ~repo ~id ()] is issue comment [id] in repo
        [user]/[repo]. */

    let get_comment:
      (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
      Monad.t(Response.t(Github_t.issue_comment));

    /** [update_comment ~user ~repo ~id ~body ()] is issue comment
        [id] in repo [user]/[repo] updated with content [body]. */

    let update_comment:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~id: int64,
        ~body: string,
        unit
      ) =>
      Monad.t(Response.t(Github_t.issue_comment));

    /** [delete_comment ~user ~repo ~id ()] activates after issue
        comment [id] has been deleted from repo [user]/[repo]. */

    let delete_comment:
      (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
      Monad.t(Response.t(unit));

    /** [labels ~user ~repo ~num ()] is a stream of all labels
        applied to issue [num] in the repo [user]/[repo]. */

    let labels:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Stream.t(Github_t.label);

    /** [add_labels ~user ~repo ~num ~labels ()] is the list of labels
        on issue [user]/[repo]#[num] with labels [labels] added. */

    let add_labels:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~num: int,
        ~labels: list(string),
        unit
      ) =>
      Monad.t(Response.t(list(Github_t.label)));

    /** [remove_label ~user ~repo ~num ~name ()] is the list of labels
        on issue [user]/[repo]#[num] after [name] has been removed. */

    let remove_label:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~num: int,
        ~name: string,
        unit
      ) =>
      Monad.t(Response.t(list(Github_t.label)));

    /** [replace_labels ~user ~repo ~num ~labels ()] is the list of
         labels on issue [user]/[repo]#[num] as provided by
         [labels]. */

    let replace_labels:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~num: int,
        ~labels: list(string),
        unit
      ) =>
      Monad.t(Response.t(list(Github_t.label)));

    /** [remove_labels ~user ~repo ~num ()] activates after all labels
        have been removed from issue [user]/[repo]#[num]. */

    let remove_labels:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Monad.t(Response.t(unit));

    /** [is_issue issue] is true if [issue] is an actual issue and not
        a pull request. */

    let is_issue: Github_t.issue => bool;

    /** [is_pull issue] is true if [issue] is a pull request. */

    let is_pull: Github_t.issue => bool;
  };

  /** The [Label] module exposes Github's
      {{:https://docs.github.com/rest/reference/issues#labels}labels
      API}. */

  module Label: {
    /** [for_repo ~user ~repo ()] is a stream of all labels in repo
        [user]/[repo]. */

    let for_repo:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.label);

    /** [get ~user ~repo ~name ()] is the label [name] in the repo
        [user]/[repo]. */

    let get:
      (~token: Token.t=?, ~user: string, ~repo: string, ~name: string, unit) =>
      Monad.t(Response.t(Github_t.label));

    /** [create ~user ~repo ~label ()] is the newly created label
        [label] in the repo [user]/[repo]. */

    let create:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~label: Github_t.new_label,
        unit
      ) =>
      Monad.t(Response.t(Github_t.label));

    /** [update ~user ~repo ~name ()] is the newly updated label
        [name] with properties [label] in the repo [user]/[repo]. */

    let update:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~name: string,
        ~label: Github_t.new_label,
        unit
      ) =>
      Monad.t(Response.t(Github_t.label));

    /** [delete ~user ~repo ~name ()] activates after the label [name]
          in the repo [user]/[repo] has been removed. */

    let delete:
      (~token: Token.t=?, ~user: string, ~repo: string, ~name: string, unit) =>
      Monad.t(Response.t(unit));
  };

  /** The [Collaborator] module exposes Github's
      {{:https://docs.github.com/rest/reference/repos#collaborators}
      collaborators API}. */

  module Collaborator: {
    /** [for_repo ~user ~repo ()] is a stream of all collaborators in repo
        [user]/[repo]. */

    let for_repo:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.linked_user);

    /** [exists ~user ~repo ~name ()] is true if [name] is a collaborator on
        repo [user]/[repo]. */

    let exists:
      (~token: Token.t=?, ~user: string, ~repo: string, ~name: string, unit) =>
      Monad.t(Response.t(bool));

    /** [add ~user ~repo ~name ?permission ()] adds [name] as a collaborator on
        repo [user]/[repo] with permission [permission] (default [`Push]). */

    let add:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~name: string,
        ~permission: Github_t.team_permission=?,
        unit
      ) =>
      Monad.t(Response.t(unit));

    /** [remove ~user ~repo ~name ()] activates after [name] has been
        removed from the collaborator set on the repo [user]/[repo]. */

    let remove:
      (~token: Token.t=?, ~user: string, ~repo: string, ~name: string, unit) =>
      Monad.t(Response.t(unit));
  };

  /** The [Milestone] module exposes GitHub's
      {{:https://docs.github.com/rest/reference/issues#milestones}milestone
      API}. */

  module Milestone: {
    /** [for_repo ?state ?sort ?direction ~user ~repo ()] is a stream
        of all milestones in repo [user]/[repo] which match [?state]
        (default [`Open]). The stream is sorted by [?sort] (default
        [`Due_date]) and ordered by [?direction] (default [`Desc]). */

    let for_repo:
      (
        ~token: Token.t=?,
        ~state: Filter.state=?,
        ~sort: Filter.milestone_sort=?,
        ~direction: Filter.direction=?,
        ~user: string,
        ~repo: string,
        unit
      ) =>
      Stream.t(Github_t.milestone);

    /** [get ~user ~repo ~num ()] is milestone number [num] in repo
        [user]/[repo]. */

    let get:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Monad.t(Response.t(Github_t.milestone));

    /** [create ~user ~repo ~milestone ()] is the newly created
        milestone described by [milestone] in repo [user]/[repo]. */

    let create:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~milestone: Github_t.new_milestone,
        unit
      ) =>
      Monad.t(Response.t(Github_t.milestone));

    /** [delete ~user ~repo ~num ()] activates after milestone
        [num] in repo [user]/[repo] has been deleted. */

    let delete:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Monad.t(Response.t(unit));

    /** [update ~user ~repo ~milestone ~num ()] is the updated
        milestone [num] in repo [user]/[repo] as described by
        [milestone]. */

    let update:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~milestone: Github_t.update_milestone,
        ~num: int,
        unit
      ) =>
      Monad.t(Response.t(Github_t.milestone));

    /** [labels ~user ~repo ~num ()] is a stream of all labels for
        milestone [num] in repo [user]/[repo]. */

    let labels:
      (~token: Token.t=?, ~user: string, ~repo: string, ~num: int, unit) =>
      Stream.t(Github_t.label);
  };

  /** The [Release] module provides access to GitHub's
      {{:https://docs.github.com/rest/reference/repos#releases}release API}
      features. */

  module Release: {
    /** [for_repo ~user ~repo ()] is a stream of all releases in repo
        [user]/[repo]. */

    let for_repo:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.release);

    /** [get ~user ~repo ~id ()] is release number [id] in repo
        [user]/[repo]. */

    let get:
      (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
      Monad.t(Response.t(Github_t.release));

    /** [get_by_tag_name ~user ~repo ~tag ()] is the release in repo
        [user]/[repo] which is using git tag [tag]. */

    let get_by_tag_name:
      (~token: Token.t=?, ~user: string, ~repo: string, ~tag: string, unit) =>
      Monad.t(Response.t(Github_t.release));

    /** [get_latest ~user ~repo ()] is the latest published full release
        in [user]/[repo]. */

    let get_latest:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Monad.t(Response.t(Github_t.release));

    /** [create ~user ~repo ~release ()] is the newly created release
        described by [release] in repo [user]/[repo]. */

    let create:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~release: Github_t.new_release,
        unit
      ) =>
      Monad.t(Response.t(Github_t.release));

    /** [delete ~user ~repo ~id ()] activates after release [id]
        in repo [user]/[repo] has been deleted. */

    let delete:
      (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
      Monad.t(Response.t(unit));

    /** [update ~user ~repo ~release ~id ()] is the updated release
        [id] in [user]/[repo] as described by [release]. */

    let update:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~release: Github_t.update_release,
        ~id: int64,
        unit
      ) =>
      Monad.t(Response.t(Github_t.release));

    /** [list_assets ~user ~repo ~id ()] lists the assets in release
        [id] in [user]/[repo]. */

    let list_assets:
      (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
      Monad.t(Response.t(Github_t.release_assets));

    /** [get_asset ~user ~repo ~id ()] gets an asset from a release
        [id] in [user]/[repo]. */

    let get_asset:
      (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
      Monad.t(Response.t(Github_t.release_asset));

    /** [delete_asset ~user ~repo ~id ()] deletes an asset from a release
        [id] in [user]/[repo]. */

    let delete_asset:
      (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
      Monad.t(Response.t(unit));

    /** [upload_asset ~user ~repo ~id ~filename ~content_type ~body ()]
        activates after [body] is uploaded to repo [user]/[repo] as
        an asset for release [id] with file name [filename] and content
        type [content_type]. */

    let upload_asset:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~id: int64,
        ~filename: string,
        ~content_type: string,
        ~body: string,
        unit
      ) =>
      Monad.t(Response.t(unit));
  };

  /** The [Deploy_key] module provides the means to manage
      per-repository
      {{:https://docs.github.com/developers/overview/managing-deploy-keys#deploy-keys}
      deploy keys}.
      @see <https://docs.github.com/rest/reference/repos#deploy-keys> deploy key
      API docs
  */

  module Deploy_key: {
    /** [for_repo ~user ~repo ()] is a stream of deploy keys
        associated with repo [user]/[repo]. */

    let for_repo:
      (~token: Token.t=?, ~user: string, ~repo: string, unit) =>
      Stream.t(Github_t.deploy_key);

    /** [get ~user ~repo ~id ()] is deploy key [id] for repo [user]/[repo]. */

    let get:
      (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
      Monad.t(Response.t(Github_t.deploy_key));

    /** [create ~user ~repo ~new_key ()] is the newly created deploy
        key [new_key] for repo [user]/[repo]. */

    let create:
      (
        ~token: Token.t=?,
        ~user: string,
        ~repo: string,
        ~new_key: Github_t.new_deploy_key,
        unit
      ) =>
      Monad.t(Response.t(Github_t.deploy_key));

    /** [delete ~user ~repo ~id ()] activates after deploy key
        [id] in repo [user]/[repo] has been deleted. */

    let delete:
      (~token: Token.t=?, ~user: string, ~repo: string, ~id: int64, unit) =>
      Monad.t(Response.t(unit));
  };

  /** The [Gist] module provides access to the GitHub
      {{:https://docs.github.com/rest/reference/gists}gist API}. */

  module Gist: {
    /** [for_user ?since ~user ()] is a stream of gists that belong to
        [user]. If [?since] is an ISO 8601 timestamp, only gists
        updated since this time are returned. */

    let for_user:
      (~token: Token.t=?, ~since: string=?, ~user: string, unit) =>
      Stream.t(Github_t.gist);

    /** [all ?since ()] is a stream of all of the gists for the
        current token's user or all public gists if invoked without a
        current token. If [?since] is an ISO 8601 timestamp, only gists
        updated since this time are returned. */

    let all:
      (~token: Token.t=?, ~since: string=?, unit) => Stream.t(Github_t.gist);

    /** [all_public ?since ()] is a stream of all of the public gists for the
        current token's user or all public gists if invoked without a current
        token. If [?since] is an ISO 8601 timestamp, only gists updated since
        this time are returned. */

    let all_public:
      (~token: Token.t=?, ~since: string=?, unit) => Stream.t(Github_t.gist);

    /** [starred ?since ()] is a stream of all starred gists for the
        current token's user. If [?since] is an ISO 8601 timestamp, only gists
        updated since this time are returned. */

    let starred:
      (~token: Token.t=?, ~since: string=?, unit) => Stream.t(Github_t.gist);

    /** [get ~id ()] is the gist [id]. */

    let get:
      (~token: Token.t=?, ~id: string, unit) =>
      Monad.t(Response.t(Github_t.gist));

    /** [create ~gist ()] is a newly created gist described by [gist]. */

    let create:
      (~token: Token.t=?, ~gist: Github_t.new_gist, unit) =>
      Monad.t(Response.t(Github_t.gist));

    /** [update ~id ~gist ()] is the updated gist [id] as described
        by [gist]. */

    let update:
      (~token: Token.t=?, ~id: string, ~gist: Github_t.update_gist, unit) =>
      Monad.t(Response.t(Github_t.gist));

    /** [commits ~id ()] is a stream of commits for gist [id]. */

    let commits:
      (~token: Token.t=?, ~id: string, unit) => Stream.t(Github_t.gist_commit);

    /** [star ~id ()] activates after gist [id] is marked as
        starred by the current token's user. */

    let star:
      (~token: Token.t=?, ~id: string, unit) => Monad.t(Response.t(unit));

    /** [unstar ~id ()] activates after gist [id] is marked as
        not starred by the current token's user. */

    let unstar:
      (~token: Token.t=?, ~id: string, unit) => Monad.t(Response.t(unit));

    /* is_starred */

    /** [fork ~id ()] is a newly forked gist from gist [id]. */

    let fork:
      (~token: Token.t=?, ~id: string, unit) =>
      Monad.t(Response.t(Github_t.gist));

    /** [forks ~id ()] is a stream of forks of gist [id]. */

    let forks:
      (~token: Token.t=?, ~id: string, unit) => Stream.t(Github_t.gist_fork);

    /** [delete ~id ()] activates after gist [id] has been deleted. */

    let delete:
      (~token: Token.t=?, ~id: string, unit) => Monad.t(Response.t(unit));
  };

  /** The [Emoji] module exposes GitHub's
      {{:https://docs.github.com/rest/reference/emojis}emoji API}. */

  module Emoji: {
    /** [list ()] is the list of all available emojis for use on
        GitHub in GitHub-flavored markdown. */

    let list:
      (~token: Token.t=?, unit) => Monad.t(Response.t(Github_t.emojis));
  };

  /** The [Check] module exposes Github's
      {{:https://docs.github.com/en/rest/reference/checks}checks API}. */

  module Check: {
    /** [create_check_run ~owner ~repo ~body] creates a new check run for a specified commit in a repository.

        See {{:https://docs.github.com/en/rest/reference/checks#create-a-check-run}create-a-check-run}.
    */

    let create_check_run:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~body: string, /* JSON type here for body*/
        unit
      ) =>
      Monad.t(Response.t(Github_t.check_run));

    /** [update_check_run ~owner ~repo ~check_run_id ~body] for a specified [check_run_id] in a repository.

        See {{:https://docs.github.com/en/rest/reference/checks#update-a-check-run}update-a-check-run}.
    */

    let update_check_run:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~check_run_id: string,
        ~body: string, /* JSON type here for body*/
        unit
      ) =>
      Monad.t(Response.t(Github_t.check_run));

    /** [get_check_run ~owner ~repo ~check_run_id] using its [check_run_id] in a repository.

        See {{:https://docs.github.com/en/rest/reference/checks#get-a-check-run}get-a-check-run}.
    */

    let get_check_run:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~check_run_id: string,
        unit
      ) =>
      Monad.t(Response.t(Github_t.check_run));

    /** [list_annotations ~owner ~repo ~check_run_id] for a check run using the annotation [check_run_id].

        See {{:https://docs.github.com/en/rest/reference/checks#list-check-run-annotations}list-check-run-annotations}.
    */

    let list_annotations:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~check_run_id: string,
        unit
      ) =>
      Monad.t(Response.t(Github_t.check_run_annotations));

    /** [list_check_runs ~owner ~repo ~check_suite_id] in a check suite using its [check_suite_id].

        See {{:https://docs.github.com/en/rest/reference/checks#list-check-runs-in-a-check-suite} list-check-runs-in-a-check-suite}.
    */

    let list_check_runs:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~check_suite_id: string,
        unit
      ) =>
      Monad.t(Response.t(Github_t.check_runs_list));

    /** [list_check_runs_for_ref ~owner ~repo ~sha], the [sha] can be a SHA, branch name, or a tag name.

          See {{:https://docs.github.com/en/rest/reference/checks#list-check-runs-for-a-git-reference}list-check-runs-for-a-git-reference}.
    */

    let list_check_runs_for_ref:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~sha: string,
        ~check_name: string=?,
        ~app_id: string=?,
        ~status: string=?,
        unit
      ) =>
      Monad.t(Response.t(Github_t.check_runs_list));

    /** [create_check_suite ~owner ~repo ~body] where body is the sha of the head commit.

        See {{:https://docs.github.com/en/rest/reference/checks#create-a-check-suite}create-a-check-suite}.
     */

    let create_check_suite:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~body: string,
        unit
      ) =>
      Monad.t(Response.t(Github_t.check_suite));

    /** [update_preferences_for_check_suites ~owner ~repo ~body] changes the default automatic flow when creating check suites.
        Where [body] contains an array of auto_trigger_checks

        See {{:https://docs.github.com/en/rest/reference/checks#update-repository-preferences-for-check-suites}update-repository-preferences-for-check-suites}.
     */

    let update_preferences_for_check_suites:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~body: string,
        unit
      ) =>
      Monad.t(Response.t(Github_t.check_suite_preferences));

    /** [get_check_suite ~owner ~repo ~check_suite_id] retrieves a single check_suite using its [check_suite_id]

        See {{:https://docs.github.com/en/rest/reference/checks#get-a-check-suite}get-a-check-suite}.
     */

    let get_check_suite:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~check_suite_id: int,
        unit
      ) =>
      Monad.t(Response.t(Github_t.check_suite));

    /** [rerequest_check_suite ~owner ~repo ~check_suite_id] triggers GitHub to rerequest an existing check suite,
        without pushing new code to a repository.

        See {{:https://docs.github.com/en/rest/reference/checks#rerequest-a-check-suite}rerequest-a-check-suite}.
     */

    let rerequest_check_suite:
      (
        ~token: Token.t=?,
        ~owner: string,
        ~repo: string,
        ~check_suite_id: int,
        unit
      ) =>
      Monad.t(Response.t(unit));

    /** [list_check_suites_for_ref ~owner ~repo ~sha] lists check suites for a commit [sha].

     See {{:https://docs.github.com/en/rest/reference/checks#list-check-suites-for-a-git-reference}list-check-suites-for-a-git-reference}.
     */

    let list_check_suites_for_ref:
      (~token: Token.t=?, ~owner: string, ~repo: string, ~sha: string, unit) =>
      Monad.t(Response.t(Github_t.check_suite_list));
  };

  /** The [Search] module exposes GitHub's
      {{:https://docs.github.com/rest/reference/search}search interfaces}. */

  module Search: {
    /** [repos ?sort ?direction ~qualifiers ~keywords ()] is a
        stream of repository search results for [keywords] and
        matching [qualifiers] predicates. Results are sorted by
        [?sort] (default best match) and ordered by [?direction]
        (default [`Desc]). */

    let repos:
      (
        ~token: Token.t=?,
        ~sort: Filter.repo_sort=?,
        ~direction: Filter.direction=?,
        ~qualifiers: list(Filter.qualifier),
        ~keywords: list(string),
        unit
      ) =>
      Stream.t(Github_t.repository_search);

    /** [issues ?sort ?direction ~qualifiers ~keywords ()] is a
        stream of issue search results for [keywords] and
        matching [qualifiers] predicates. Results are sorted by
        [?sort] (default best match) and ordered by [?direction]
        (default [`Desc]). */

    let issues:
      (
        ~token: Token.t=?,
        ~sort: Filter.repo_sort=?,
        ~direction: Filter.direction=?,
        ~qualifiers: list(Filter.issue_qualifier),
        ~keywords: list(string),
        unit
      ) =>
      Stream.t(Github_t.repository_issue_search);
  };

  /** {4 Utility Modules} */;

  /** The [Git_obj] module contains utility functions for working with
      git concepts. */

  module Git_obj: {
    /** [type_to_string type] is the string name of object type [type]. */

    let type_to_string: Github_t.obj_type => string;

    /** [split_ref ref] is the pair of ref directory and ref
        name. E.g. if [ref] is "refs/tags/foo/bar" then [split_ref
        ref] is ("tags","foo/bar"). */

    let split_ref: string => (string, string);
  };
};

/** A module of this type is required in order to construct a
    {!Github} module using {!Github_core.Make}. */

module type Env = {
  /** [debug] is the initial debugging flag value. */

  let debug: bool;
};

/** A module of this type is required in order to construct a
    {!Github} module using {!Github_core.Make}. */

module type Time = {
  /** [now ()] is the current UNIX epoch time in seconds. */

  let now: unit => float;

  /** [sleep sec] activates after [sec] seconds have elapsed. */

  let sleep: float => Lwt.t(unit);
};
