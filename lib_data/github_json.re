/*
    Adapters used by atdgen to turn Github's representation of variants
    into an ATD-compatible representation.
 */
module Adapter = {
  module Ref =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make({
      let type_field_name = "ref_type";
      let value_field_name = "ref";
      let known_tags = None;
    });

  module Payload =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make({
      let type_field_name = "type";
      let value_field_name = "payload";
      let known_tags = None;
    });

  module Issue_comment_event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make({
      let type_field_name = "action";
      let value_field_name = "changes";
      let known_tags = Some((["created", "edited", "deleted"], "Unknown"));
    });

  module Issues_event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make({
      let type_field_name = "action";
      let value_field_name = "changes";
      let known_tags =
        Some((
          [
            "assigned",
            "unassigned",
            "labeled",
            "unlabeled",
            "opened",
            "edited",
            "closed",
            "reopened",
          ],
          "Unknown",
        ));
    });

  module Pull_request_event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make({
      let type_field_name = "action";
      let value_field_name = "changes";
      let known_tags =
        Some((
          [
            "assigned",
            "unassigned",
            "labeled",
            "unlabeled",
            "opened",
            "edited",
            "closed",
            "reopened",
            "synchronize",
          ],
          "Unknown",
        ));
    });

  module Pull_request_review_comment_event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make({
      let type_field_name = "action";
      let value_field_name = "changes";
      let known_tags = Some((["created", "edited", "deleted"], "Unknown"));
    });

  module Event =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make({
      let type_field_name = "type";
      let value_field_name = "payload";
      let known_tags =
        Some((
          [
            "CommitCommentEvent",
            "CreateEvent",
            "DeleteEvent",
            "DownloadEvent",
            "FollowEvent",
            "ForkEvent",
            "ForkApplyEvent",
            "GistEvent",
            "GollumEvent",
            "IssueCommentEvent",
            "IssuesEvent",
            "MemberEvent",
            "PublicEvent",
            "PullRequestEvent",
            "PullRequestReviewCommentEvent",
            "PushEvent",
            "ReleaseEvent",
            "RepositoryEvent",
            "StatusEvent",
            "WatchEvent",
          ],
          "Unknown",
        ));
    });

  module Hook =
    Atdgen_runtime.Json_adapter.Type_and_value_fields.Make({
      let type_field_name = "name";
      let value_field_name = "config";
      let known_tags = Some((["web"], "Unknown"));
    });
};
