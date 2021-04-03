module Github exposing
    ( AuthToken, authToken
    , getBranch, listTags, createBranch
    , getCommit, createCommit
    , PullRequest, getPullRequests, getPullRequest, createPullRequest
    , getFileContents, updateFileContents
    , createFork
    , getComments, createComment
    , getBranchZip, getCommitZip, getRepository, getTag, updateBranch
    )

{-|

@docs AuthToken, authToken
@docs getBranch, listTags, createBranch
@docs getCommit, createCommit
@docs PullRequest, getPullRequests, getPullRequest, createPullRequest
@docs getFileContents, updateFileContents
@docs createFork


## Issues

@docs getComments, createComment

-}

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode
import Http
import Iso8601
import Json.Decode
import Json.Encode
import Task exposing (Task)
import Time


{-| See <https://docs.github.com/en/rest/reference/repos#get-a-repository>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getRepository :
    { authToken : AuthToken
    , owner : String
    , repo : String
    }
    -> Task Http.Error { defaultBranch : String }
getRepository params =
    let
        decoder =
            Json.Decode.map (\defaultBranch -> { defaultBranch = defaultBranch })
                (Json.Decode.field "default_branch" Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.owner ++ "/" ++ params.repo
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


getBranchZip : { authToken : AuthToken, owner : String, repo : String, branchName : String } -> Task Http.Error Bytes
getBranchZip params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://github.com/" ++ params.owner ++ "/" ++ params.repo ++ "/archive/refs/heads/" ++ params.branchName ++ ".zip"
        , body = Http.emptyBody
        , resolver = bytesResolver
        , timeout = Nothing
        }


getCommitZip : { authToken : AuthToken, owner : String, repo : String, sha : String } -> Task Http.Error Bytes
getCommitZip params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://github.com/" ++ params.owner ++ "/" ++ params.repo ++ "/archive/" ++ params.sha ++ ".zip"
        , body = Http.emptyBody
        , resolver = bytesResolver
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/git/commits/#get-a-commit>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getCommit :
    { authToken : AuthToken
    , repo : String
    , sha : String
    }
    ->
        Task
            Http.Error
            { sha : String
            , tree :
                { sha : String
                }
            }
getCommit params =
    let
        decoder =
            Json.Decode.map2
                (\sha treeSha ->
                    { sha = sha
                    , tree = { sha = treeSha }
                    }
                )
                (Json.Decode.at [ "sha" ] Json.Decode.string)
                (Json.Decode.at [ "tree", "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/git/commits/" ++ params.sha
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#create-a-commit>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createCommit :
    { authToken : AuthToken
    , owner : String
    , repo : String
    , message : String
    , tree : String
    , parents : List String
    }
    ->
        Task
            Http.Error
            { sha : String
            }
createCommit params =
    let
        decoder =
            Json.Decode.at [ "sha" ] Json.Decode.string
                |> Json.Decode.map (\sha -> { sha = sha })
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.owner ++ "/" ++ params.repo ++ "/git/commits"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "message", Json.Encode.string params.message )
                    , ( "tree", Json.Encode.string params.tree )
                    , ( "parents", Json.Encode.list Json.Encode.string params.parents )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#get-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getBranch :
    { authToken : AuthToken
    , owner : String
    , repo : String
    , branchName : String
    }
    ->
        Task
            Http.Error
            { object :
                { sha : String
                }
            }
getBranch params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.owner ++ "/" ++ params.repo ++ "/git/refs/heads/" ++ params.branchName
        , body = Http.emptyBody
        , resolver = jsonResolver referenceDecoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#update-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
updateBranch :
    { authToken : AuthToken
    , owner : String
    , repo : String
    , branchName : String
    , sha : String
    , force : Bool
    }
    ->
        Task
            Http.Error
            { object :
                { sha : String
                }
            }
updateBranch params =
    Http.task
        { method = "PATCH"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.owner ++ "/" ++ params.repo ++ "/git/refs/heads/" ++ params.branchName
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "sha", Json.Encode.string params.sha ), ( "force", Json.Encode.bool params.force ) ]
                )
        , resolver = jsonResolver referenceDecoder
        , timeout = Nothing
        }


{-| See <https://docs.github.com/en/rest/reference/git#get-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getTag :
    { authToken : AuthToken
    , owner : String
    , repo : String
    , tagName : String
    }
    ->
        Task
            Http.Error
            { object :
                { sha : String
                }
            }
getTag params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.owner ++ "/" ++ params.repo ++ "/git/refs/tags/" ++ params.tagName
        , body = Http.emptyBody
        , resolver = jsonResolver referenceDecoder
        , timeout = Nothing
        }


referenceDecoder =
    Json.Decode.at [ "object", "sha" ] Json.Decode.string
        |> Json.Decode.map (\sha -> { object = { sha = sha } })


type alias Tag =
    { name : String
    , commit : { sha : String, url : String }
    , zipballUrl : String
    , tarballUrl : String
    , nodeId : String
    }


decodeTag : Json.Decode.Decoder Tag
decodeTag =
    Json.Decode.map5 Tag
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "commit"
            (Json.Decode.map2 (\sha url -> { sha = sha, url = url })
                (Json.Decode.field "sha" Json.Decode.string)
                (Json.Decode.field "url" Json.Decode.string)
            )
        )
        (Json.Decode.field "zipball_url" Json.Decode.string)
        (Json.Decode.field "tarball_url" Json.Decode.string)
        (Json.Decode.field "node_id" Json.Decode.string)


{-| See <https://docs.github.com/en/rest/reference/repos#list-repository-tags>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
listTags :
    { authToken : AuthToken
    , owner : String
    , repo : String
    }
    -> Task Http.Error (List Tag)
listTags params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.owner ++ "/" ++ params.repo ++ "/tags"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decodeTag)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/git/refs/#create-a-reference>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createBranch :
    { authToken : AuthToken
    , repo : String
    , branchName : String
    , sha : String
    }
    -> Task Http.Error ()
createBranch params =
    let
        decoder =
            Json.Decode.succeed ()
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/git/refs"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "ref", Json.Encode.string ("refs/heads/" ++ params.branchName) )
                    , ( "sha", Json.Encode.string params.sha )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| The data returned by [`getPullRequests`](#getPullRequests).
-}
type alias PullRequest =
    { number : Int
    , title : String
    }


decodePullRequest =
    Json.Decode.map2
        PullRequest
        (Json.Decode.at [ "number" ] Json.Decode.int)
        (Json.Decode.at [ "title" ] Json.Decode.string)


{-| See <https://developer.github.com/v3/pulls/#list-pull-requests>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getPullRequests :
    { authToken : AuthToken
    , repo : String
    }
    -> Task Http.Error (List PullRequest)
getPullRequests params =
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/pulls"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decodePullRequest)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getPullRequest :
    { authToken : AuthToken
    , repo : String
    , number : Int
    }
    ->
        Task
            Http.Error
            { head :
                { ref : String
                , sha : String
                }
            }
getPullRequest params =
    let
        decoder =
            Json.Decode.map2
                (\headRef headSha ->
                    { head =
                        { ref = headRef
                        , sha = headSha
                        }
                    }
                )
                (Json.Decode.at [ "head", "ref" ] Json.Decode.string)
                (Json.Decode.at [ "head", "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/pulls/" ++ String.fromInt params.number
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/pulls/#create-a-pull-request>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createPullRequest :
    { authToken : AuthToken
    , owner : String
    , repo : String
    , baseBranchOwner : String
    , branchName : String
    , baseBranch : String
    , title : String
    , description : String
    }
    -> Task Http.Error ()
createPullRequest params =
    let
        decoder =
            Json.Decode.succeed ()
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.owner ++ "/" ++ params.repo ++ "/pulls"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "title", Json.Encode.string params.title )
                    , ( "head", Json.Encode.string params.branchName )
                    , ( "base"
                      , Json.Encode.string
                            (if params.owner == params.baseBranchOwner then
                                params.baseBranch

                             else
                                params.baseBranchOwner ++ ":" ++ params.baseBranch
                            )
                      )
                    , ( "body", Json.Encode.string params.description )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/repos/contents/#get-contents>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getFileContents :
    { authToken : AuthToken
    , repo : String
    , ref : String
    , path : String
    }
    ->
        Task
            Http.Error
            { encoding : String
            , content : String
            , sha : String
            }
getFileContents params =
    let
        decoder =
            Json.Decode.map3
                (\encoding content sha ->
                    { encoding = encoding
                    , content = content
                    , sha = sha
                    }
                )
                (Json.Decode.at [ "encoding" ] Json.Decode.string)
                (Json.Decode.at [ "content" ] Json.Decode.string)
                (Json.Decode.at [ "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/contents/" ++ params.path ++ "?ref=" ++ params.ref
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/repos/contents/#update-a-file>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
updateFileContents :
    { authToken : AuthToken
    , repo : String
    , branch : String
    , path : String
    , sha : String
    , message : String
    , content : String
    }
    ->
        Task
            Http.Error
            { content :
                { sha : String
                }
            }
updateFileContents params =
    let
        decoder =
            Json.Decode.map
                (\contentSha ->
                    { content = { sha = contentSha } }
                )
                (Json.Decode.at [ "content", "sha" ] Json.Decode.string)
    in
    Http.task
        { method = "PUT"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/contents/" ++ params.path
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "message", Json.Encode.string params.message )
                    , ( "content", Json.Encode.string (Base64.encode params.content) )
                    , ( "sha", Json.Encode.string params.sha )
                    , ( "branch", Json.Encode.string params.branch )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
getComments :
    { authToken : AuthToken
    , repo : String
    , issueNumber : Int
    }
    ->
        Task
            Http.Error
            (List
                { body : String
                , user :
                    { login : String
                    , avatarUrl : String
                    }
                , createdAt : Time.Posix
                , updatedAt : Time.Posix
                }
            )
getComments params =
    let
        decoder =
            Json.Decode.map5
                (\body userLogin userAvatarUrl createdAt updatedAt ->
                    { body = body
                    , user =
                        { login = userLogin
                        , avatarUrl = userAvatarUrl
                        }
                    , createdAt = createdAt
                    , updatedAt = updatedAt
                    }
                )
                (Json.Decode.at [ "body" ] Json.Decode.string)
                (Json.Decode.at [ "user", "login" ] Json.Decode.string)
                (Json.Decode.at [ "user", "avatar_url" ] Json.Decode.string)
                (Json.Decode.at [ "created_at" ] Iso8601.decoder)
                (Json.Decode.at [ "updated_at" ] Iso8601.decoder)
    in
    Http.task
        { method = "GET"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/issues/" ++ String.fromInt params.issueNumber ++ "/comments"
        , body = Http.emptyBody
        , resolver = jsonResolver (Json.Decode.list decoder)
        , timeout = Nothing
        }


{-| See <https://developer.github.com/v3/issues/comments/#create-a-comment>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createComment :
    { authToken : AuthToken
    , repo : String
    , issueNumber : Int
    , body : String
    }
    ->
        Task
            Http.Error
            { body : String
            , user :
                { login : String
                , avatarUrl : String
                }
            , createdAt : Time.Posix
            , updatedAt : Time.Posix
            }
createComment params =
    let
        decoder =
            Json.Decode.map5
                (\body userLogin userAvatarUrl createdAt updatedAt ->
                    { body = body
                    , user =
                        { login = userLogin
                        , avatarUrl = userAvatarUrl
                        }
                    , createdAt = createdAt
                    , updatedAt = updatedAt
                    }
                )
                (Json.Decode.at [ "body" ] Json.Decode.string)
                (Json.Decode.at [ "user", "login" ] Json.Decode.string)
                (Json.Decode.at [ "user", "avatar_url" ] Json.Decode.string)
                (Json.Decode.at [ "created_at" ] Iso8601.decoder)
                (Json.Decode.at [ "updated_at" ] Iso8601.decoder)
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.repo ++ "/issues/" ++ String.fromInt params.issueNumber ++ "/comments"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "body", Json.Encode.string params.body )
                    ]
                )
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


type AuthToken
    = AuthToken String


authToken : String -> AuthToken
authToken =
    AuthToken


{-| See <https://docs.github.com/en/rest/reference/repos#create-a-fork>

NOTE: Not all input options and output fields are supported yet. Pull requests adding more complete support are welcome.

-}
createFork :
    { authToken : AuthToken
    , owner : String
    , repo : String
    }
    ->
        Task
            Http.Error
            { owner : String
            , repo : String
            }
createFork params =
    let
        decoder =
            Json.Decode.map2
                (\owner repo ->
                    { owner = owner
                    , repo = repo
                    }
                )
                (Json.Decode.at [ "owner", "login" ] Json.Decode.string)
                (Json.Decode.at [ "name" ] Json.Decode.string)
    in
    Http.task
        { method = "POST"
        , headers = [ authorizationHeader params.authToken ]
        , url = "https://api.github.com/repos/" ++ params.owner ++ "/" ++ params.repo ++ "/forks"
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


authorizationHeader : AuthToken -> Http.Header
authorizationHeader (AuthToken authToken_) =
    Http.header "Authorization" ("token " ++ authToken_)


jsonResolver : Json.Decode.Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.GoodStatus_ _ body ->
                    Json.Decode.decodeString decoder body
                        |> Result.mapError Json.Decode.errorToString
                        |> Result.mapError Http.BadBody

                Http.BadUrl_ message ->
                    Err (Http.BadUrl message)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)


bytesResolver : Http.Resolver Http.Error Bytes
bytesResolver =
    Http.bytesResolver <|
        \response ->
            case response of
                Http.GoodStatus_ _ body ->
                    Ok body

                Http.BadUrl_ message ->
                    Err (Http.BadUrl message)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)
