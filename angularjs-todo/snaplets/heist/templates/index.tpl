<apply template="base">

  <ifLoggedIn>
    <h2>Demo: Snap and sqlite-simple</h2>
    <p>
      This is a simple demo page served the <a
      href="http://snapframework.com/">Snap</a> web framework using
      the <a href="https://github.com/nurpax/snaplet-sqlite-simple">snaplet-sqlite-simple</a>
      package for database access and authentication.
    </p>

    <p>Congrats!  You're logged in as: <strong><loggedInUser/></strong></p>

    <h3>Your comments</h3>
    <ul>
    <comments>
      <li><span style="color:#888;"><savedOn/></span>: <comment/></li>
    </comments>
    </ul>

    <form method="post" action="/save_comment">
      <label>Add a comment: </label><input type="text" name="comment"/>
      <input type="submit" value="Add"/>
    </form>

    <p><a href="/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
