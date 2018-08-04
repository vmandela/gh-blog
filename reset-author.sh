git filter-branch -f --env-filter "
    GIT_AUTHOR_NAME='Venkateswara Rao Mandela'
    GIT_AUTHOR_EMAIL='venkat.mandela@gmail.com'
    GIT_COMMITTER_NAME='Venkateswara Rao Mandela'
    GIT_COMMITTER_EMAIL='venkat.mandela@gmail.com'
  " HEAD