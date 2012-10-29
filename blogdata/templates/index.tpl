
<bind tag="subtitle">: Blog</bind>
<apply template="page">

  <div id="blog-index">
    <span itemscope="itemscope" itemtype="http://data-vocabulary.org/Breadcrumb"></span>

    <h2 class="blog-heading">Latest post</h2>

    <posts:latest>
      <div class="most-recent-post">
        <h3><a href="/blog${post:url}"><post:title/></a></h3>
        <div class="post-content">
          <post:content/>
        </div>
      </div>
    </posts:latest>

    <h2 class="blog-heading">Recently</h2>

    <posts:next3>
      <div class="post">
        <div class="title">
          <h3><a href="/blog${post:url}"><post:title/></a> <span class="date"><post:date/></span></h3>
        </div>
      </div>
    </posts:next3>

  </div>
</apply>

