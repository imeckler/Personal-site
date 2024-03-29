<bind tag="subtitle">: Blog: <post:title/></bind>
<apply template="page">

  <div id="blog-post" class="main-post">

    <ignore>
      <h2 class="crumb-title">
        <span itemscope="itemscope" itemtype="http://data-vocabulary.org/Breadcrumb">
          <a itemprop="url" href="/blog"><span itemprop="title">Blog</span></a>
        </span>
        <span class="crumb-divider" >&raquo;</span>
        <span itemscope="itemscope" itemtype="http://data-vocabulary.org/Breadcrumb">
          <a itemprop="url" href="#"><span itemprop="title"><post:title/></span></a>
        </span>
      </h2>
    </ignore>

    <div class="post-meta">
      <h3 class="post-title"><post:title/></h3>
      <div class="post-date"><post:date/></div>
      <div class="post-summary"><post:summary/></div>
    </div>

    <div class="post-content">
      <post:content/>
    </div>
  </div>

</apply>
