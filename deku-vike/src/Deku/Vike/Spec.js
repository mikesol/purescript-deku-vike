import { escapeInject, dangerouslySkipEscape } from "vike/server";
import jsdomGlobal from "jsdom-global";

 export const buildHtml = (htmlFunction) => (buildFunction) => () => {
  jsdomGlobal();

  document.getElementsByTagName("html")[0].innerHTML =
    "<head></head><body></body>";

  const cache = buildFunction();
  const html = dangerouslySkipEscape(cache.html);

  pageContext.dekuHydrationData = cache;

  return escapeInject`${htmlFunction(html)}`;
}